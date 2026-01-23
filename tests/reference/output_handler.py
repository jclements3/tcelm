"""
JEMINI HOSS Output Handler - RIPITT Compatible Format

Generates output files matching original RIPITT/HOSS format specification exactly.

Output Files:
1. *_sensors_ARM_applied.csv - Sensor measurements with full IR/satellite data (43 columns)
2. *_TrackData_MCN.csv - Kalman filter track with 9-state and 45 covariance elements
3. *_TrackMetrics_MCN.csv - Track errors vs truth with covariance

Reference: Original HOSS output_handler.py, DevSecOps Reference Design Section 4.3
"""

import numpy as np
from pathlib import Path
from typing import Dict, Any, Optional, List, Tuple
from dataclasses import dataclass
import csv
from os.path import extsep

from .tracking import TrackData, TrackMetrics, run_tracking_monte_carlo, latlon_to_ecef


# Earth constants (WGS-84)
EARTH_RADIUS_M = 6378137.0
EARTH_FLATTENING = 1.0 / 298.257223563
EARTH_E2 = 2 * EARTH_FLATTENING - EARTH_FLATTENING ** 2


def latlon_to_ecef_wgs84(lat_deg: float, lon_deg: float, alt_m: float) -> np.ndarray:
    """Convert geodetic coordinates to ECEF position (WGS-84)."""
    lat_rad = np.radians(lat_deg)
    lon_rad = np.radians(lon_deg)

    sin_lat = np.sin(lat_rad)
    N = EARTH_RADIUS_M / np.sqrt(1.0 - EARTH_E2 * sin_lat ** 2)

    x = (N + alt_m) * np.cos(lat_rad) * np.cos(lon_rad)
    y = (N + alt_m) * np.cos(lat_rad) * np.sin(lon_rad)
    z = (N * (1.0 - EARTH_E2) + alt_m) * np.sin(lat_rad)

    return np.array([x, y, z])


def ecef_to_geodetic_wgs84(x: float, y: float, z: float) -> tuple:
    """
    Convert ECEF coordinates to geodetic (WGS-84).

    Uses iterative algorithm for accurate conversion.

    Args:
        x, y, z: ECEF coordinates in meters

    Returns:
        Tuple of (lat_deg, lon_deg, alt_m)
    """
    # Longitude is straightforward
    lon_rad = np.arctan2(y, x)

    # Iterative latitude/altitude calculation
    p = np.sqrt(x**2 + y**2)
    lat_rad = np.arctan2(z, p * (1.0 - EARTH_E2))  # Initial estimate

    for _ in range(10):  # Usually converges in 2-3 iterations
        sin_lat = np.sin(lat_rad)
        N = EARTH_RADIUS_M / np.sqrt(1.0 - EARTH_E2 * sin_lat**2)
        lat_rad = np.arctan2(z + EARTH_E2 * N * sin_lat, p)

    # Altitude
    sin_lat = np.sin(lat_rad)
    cos_lat = np.cos(lat_rad)
    N = EARTH_RADIUS_M / np.sqrt(1.0 - EARTH_E2 * sin_lat**2)

    if abs(cos_lat) > 1e-10:
        alt_m = p / cos_lat - N
    else:
        alt_m = abs(z) - N * (1.0 - EARTH_E2)

    return np.degrees(lat_rad), np.degrees(lon_rad), alt_m


def compute_bearing_vector(sat_pos: np.ndarray, target_pos: np.ndarray) -> np.ndarray:
    """Compute unit bearing vector from satellite to target."""
    delta = target_pos - sat_pos
    norm = np.linalg.norm(delta)
    if norm < 1e-10:
        return np.array([0.0, 0.0, 0.0])
    return delta / norm


def compute_solar_geometry(time_sec: float, target_pos: np.ndarray) -> Tuple[float, np.ndarray]:
    """
    Compute solar angle and bearing for a given time and position.

    Returns:
        Tuple of (solar_angle_deg, solar_bearing_unit_vector)
    """
    # Simplified solar position model
    # In reality would use precise ephemeris
    days_since_epoch = time_sec / 86400.0

    # Solar longitude (simplified)
    solar_lon = (280.46 + 0.9856474 * days_since_epoch) % 360.0
    solar_lat = 0.0  # Simplified - sun near equator

    # Solar position at ~1 AU
    solar_dist = 1.496e11  # meters
    solar_pos = latlon_to_ecef_wgs84(solar_lat, solar_lon, solar_dist)

    # Bearing from target to sun
    delta = solar_pos - target_pos
    solar_bearing = delta / np.linalg.norm(delta)

    # Solar angle (angle between target-to-sun and target-to-satellite)
    # For simplicity, return angle from local vertical
    target_up = target_pos / np.linalg.norm(target_pos)
    cos_angle = np.dot(solar_bearing, target_up)
    solar_angle = np.degrees(np.arccos(np.clip(cos_angle, -1.0, 1.0)))

    return solar_angle, solar_bearing


@dataclass
class OutputHandler:
    """HOSS-compatible output file generator matching original RIPITT format."""

    output_dir: Path

    def __post_init__(self):
        """Ensure output directory exists."""
        self.output_dir = Path(self.output_dir)
        self.output_dir.mkdir(parents=True, exist_ok=True)

    def write_full_observation_matrix(self,
                                       taos_result: Dict[str, Any],
                                       hoss_result: Dict[str, Any],
                                       raptir_result: Dict[str, Any] = None,
                                       filename: str = None) -> Path:
        """
        Write RIPITT-format full observation matrix CSV.

        This produces ALL sensors × ALL time steps (e.g., 106 × 2380 = 252,280 rows),
        not just detections. This matches the main RIPITT CSV output format exactly.

        Columns (36): time_s, trgt_ecef_x/y/z_m, trgt_vel_x/y/z_mps, trgt_lat/lon/alt,
        sat_ecef_x/y/z_m, sat_lat/lon/alt_km, bearing_unit_x/y/z, range_km,
        inLOS, sunlit_bool, solar_angle, solar_bearing_x/y/z,
        sensor, platform, rad_int_1, irradiance_1, irradiance_scld_1, snr_1,
        meas_error, fov_deg, sea_deg, obj_name
        """
        meta = hoss_result.get('meta', {})
        satellites = hoss_result.get('satellites', [])
        trajectories = taos_result.get('trajectories', {})
        ir_signatures = taos_result.get('ir_signatures', {})
        if raptir_result:
            ir_signatures = raptir_result.get('ir_signatures', ir_signatures)

        if filename is None:
            obj_name = meta.get('missile', 'UNKNOWN_CLASS')
            filename = f"{obj_name}_observations.csv"

        output_path = self.output_dir / filename

        # Get trajectory times
        if not trajectories:
            return output_path

        comp_id = list(trajectories.keys())[0]
        traj = np.array(trajectories[comp_id])
        times = traj[:, 0]

        # TAOS trajectory format (from PRB templates):
        # col 0: time (sec)
        # col 1: xecfc (ECEF X in feet)
        # col 2: yecfc (ECEF Y in feet)
        # col 3: zecfc (ECEF Z - was converted from ft to km by launch.py)
        # col 4-6: velocities (xecfcdt, yecfcdt, zecfcdt)
        # col 7-9: accelerations
        # col 23: altkm (altitude in km)
        # col 24: velsi (velocity SI)
        FT_TO_M = 0.3048

        # Get IR signatures if available
        ir_sig = None
        if comp_id in ir_signatures:
            ir_sig = np.array(ir_signatures[comp_id])

        # 36 columns matching RIPITT exactly
        columns = [
            'time_s',
            'trgt_ecef_x_m', 'trgt_ecef_y_m', 'trgt_ecef_z_m',
            'trgt_vel_x_mps', 'trgt_vel_y_mps', 'trgt_vel_z_mps',
            'trgt_lat_deg', 'trgt_lon_deg', 'trgt_alt_km',
            'sat_ecef_x_m', 'sat_ecef_y_m', 'sat_ecef_z_m',
            'sat_lat_deg', 'sat_lon_deg', 'sat_alt_km',
            'bearing_unit_x', 'bearing_unit_y', 'bearing_unit_z',
            'range_km',
            'inLOS',
            'sunlit_bool',
            'solar_angle',
            'solar_bearing_unit_x', 'solar_bearing_unit_y', 'solar_bearing_unit_z',
            'sensor',
            'platform',
            'rad_int_1',
            'irradiance_1',
            'irradiance_scld_1',
            'snr_1',
            'meas_error',
            'fov_deg',
            'sea_deg',
            'obj_name'
        ]

        obj_name = meta.get('missile', 'UNKNOWN_CLASS')

        with open(output_path, 'w', newline='') as f:
            writer = csv.writer(f)
            writer.writerow(columns)

            # Pre-compute target positions and velocities for all times
            prev_pos = None
            prev_time = None
            target_data = []

            for i, t in enumerate(times):
                # TAOS outputs ECEF coordinates, not lat/lon/alt
                # col 1, 2 are ECEF X, Y in feet
                # col 3 is ECEF Z (was converted from ft to km by launch.py)
                ecef_x_m = traj[i, 1] * FT_TO_M
                ecef_y_m = traj[i, 2] * FT_TO_M
                ecef_z_m = traj[i, 3] * 1000.0  # km to m (already converted from ft to km)

                target_ecef = np.array([ecef_x_m, ecef_y_m, ecef_z_m])

                # Convert ECEF to geodetic lat/lon/alt
                lat_deg, lon_deg, alt_m = ecef_to_geodetic_wgs84(ecef_x_m, ecef_y_m, ecef_z_m)
                alt_km = alt_m / 1000.0

                # Velocity from finite difference
                if prev_pos is not None and prev_time is not None:
                    dt = t - prev_time
                    if dt > 0:
                        target_vel = (target_ecef - prev_pos) / dt
                    else:
                        target_vel = np.zeros(3)
                else:
                    target_vel = np.zeros(3)

                # IR intensity
                ir_intensity = 0.0
                if ir_sig is not None and i < len(ir_sig):
                    ir_intensity = ir_sig[i][2] if len(ir_sig[i]) > 2 else 0.0

                # Solar geometry
                solar_angle, solar_bearing = compute_solar_geometry(t, target_ecef)

                # Sunlit if above 100 km
                sunlit = alt_km > 100.0

                target_data.append({
                    'time': t,
                    'lat_deg': lat_deg,
                    'lon_deg': lon_deg,
                    'alt_km': alt_km,
                    'ecef': target_ecef,
                    'vel': target_vel,
                    'ir_intensity': ir_intensity,
                    'solar_angle': solar_angle,
                    'solar_bearing': solar_bearing,
                    'sunlit': sunlit
                })

                prev_pos = target_ecef
                prev_time = t

            # For each time step, output ALL satellites
            for td in target_data:
                t = td['time']
                target_ecef = td['ecef']
                target_vel = td['vel']
                ir_intensity = td['ir_intensity']

                for sat in satellites:
                    sat_id = sat['id']
                    sat_lat = sat['lat_deg']
                    sat_lon = sat['lon_deg']
                    sat_alt_km = sat['altitude_km']
                    sat_alt_m = sat_alt_km * 1000.0

                    sat_ecef = latlon_to_ecef_wgs84(sat_lat, sat_lon, sat_alt_m)

                    # Bearing and range
                    delta = target_ecef - sat_ecef
                    range_m = np.linalg.norm(delta)
                    range_km = range_m / 1000.0

                    if range_m > 0:
                        bearing = delta / range_m
                    else:
                        bearing = np.zeros(3)

                    # Line of sight check (simple horizon check)
                    # Target must be above horizon from satellite's perspective
                    sat_to_earth_center = -sat_ecef / np.linalg.norm(sat_ecef)
                    sat_to_target = delta / range_m if range_m > 0 else np.zeros(3)
                    cos_angle = np.dot(sat_to_earth_center, sat_to_target)
                    # Horizon angle depends on satellite altitude
                    horizon_angle = np.arccos(EARTH_RADIUS_M / (EARTH_RADIUS_M + sat_alt_m))
                    in_los = cos_angle < np.cos(np.pi/2 - horizon_angle)

                    # Irradiance at satellite
                    if range_m > 0 and ir_intensity > 0:
                        irradiance = ir_intensity / (4 * np.pi * range_m**2) * 1e-4
                    else:
                        irradiance = 0.0

                    irradiance_scaled = irradiance

                    # SNR
                    sensitivity = sat.get('sensitivity_W_per_sr', 0.01)
                    if sensitivity > 0 and irradiance > 0:
                        snr = (ir_intensity / (range_m**2)) / sensitivity if range_m > 0 else 0.0
                    else:
                        snr = 0.0

                    # Sensor parameters
                    meas_error = 5e-05
                    fov_deg = sat.get('fov_deg', 180.0)
                    sea_deg = 20.0

                    # Sensor and platform names
                    sensor_name = sat_id
                    platform_name = sat.get('platform', sat_id)

                    row = [
                        t,
                        target_ecef[0], target_ecef[1], target_ecef[2],
                        target_vel[0], target_vel[1], target_vel[2],
                        td['lat_deg'], td['lon_deg'], td['alt_km'],
                        sat_ecef[0], sat_ecef[1], sat_ecef[2],
                        sat_lat, sat_lon, sat_alt_km,
                        bearing[0], bearing[1], bearing[2],
                        range_km,
                        in_los,
                        td['sunlit'],
                        td['solar_angle'],
                        td['solar_bearing'][0], td['solar_bearing'][1], td['solar_bearing'][2],
                        sensor_name,
                        platform_name,
                        ir_intensity,
                        irradiance,
                        irradiance_scaled,
                        snr,
                        meas_error,
                        fov_deg,
                        sea_deg,
                        obj_name
                    ]

                    writer.writerow(row)

        return output_path

    def write_sensors_csv(self,
                          taos_result: Dict[str, Any],
                          hoss_result: Dict[str, Any],
                          raptir_result: Dict[str, Any] = None,
                          filename: str = None,
                          arm_mode: bool = False) -> Path:
        """
        Write RIPITT-format sensors CSV with full 43-column format.

        Columns match original HOSS output_handler.measurements_to_csv():
        time_s, trgt_ecef_x/y/z_m, trgt_vel_x/y/z_mps, trgt_lat/lon/alt,
        sat_ecef_x/y/z_m, sat_lat/lon/alt_km, bearing_unit_x/y/z, range_km,
        inLOS, sunlit_bool, solar_angle, solar_bearing_x/y/z,
        sensor, platform, rad_int_1, irradiance_1, irradiance_scld_1, snr_1,
        meas_error, fov_deg, sea_deg, obj_name,
        trgt_ecef_x/y/z_km, sat_ecef_x/y/z_km, status

        Args:
            taos_result: Output from TAOS with trajectories
            hoss_result: Output from HOSS track() function
            raptir_result: Output from RAPTIR with IR signatures
            filename: Output filename
            arm_mode: If True, suffix with _ARM_applied

        Returns:
            Path to written file
        """
        meta = hoss_result.get('meta', {})
        detections = hoss_result.get('detections', [])
        satellites = hoss_result.get('satellites', [])

        if filename is None:
            obj_name = meta.get('missile', 'UNKNOWN_CLASS')
            suffix = '_sensors_ARM_applied' if arm_mode else '_sensors'
            filename = f"{obj_name}{suffix}.csv"

        output_path = self.output_dir / filename

        # Build satellite lookup
        sat_lookup = {sat['id']: sat for sat in satellites}

        # Build trajectory lookup for velocity estimation
        trajectories = taos_result.get('trajectories', {})

        # Build IR signature lookup
        ir_signatures = {}
        if raptir_result:
            ir_signatures = raptir_result.get('ir_signatures', {})

        # CSV columns matching original RIPITT format exactly
        columns = [
            'time_s',
            'trgt_ecef_x_m', 'trgt_ecef_y_m', 'trgt_ecef_z_m',
            'trgt_vel_x_mps', 'trgt_vel_y_mps', 'trgt_vel_z_mps',
            'trgt_lat_deg', 'trgt_lon_deg', 'trgt_alt_km',
            'sat_ecef_x_m', 'sat_ecef_y_m', 'sat_ecef_z_m',
            'sat_lat_deg', 'sat_lon_deg', 'sat_alt_km',
            'bearing_unit_x', 'bearing_unit_y', 'bearing_unit_z',
            'range_km',
            'inLOS',
            'sunlit_bool',
            'solar_angle',
            'solar_bearing_unit_x', 'solar_bearing_unit_y', 'solar_bearing_unit_z',
            'sensor',
            'platform',
            'rad_int_1',
            'irradiance_1',
            'irradiance_scld_1',
            'snr_1',
            'meas_error',
            'fov_deg',
            'sea_deg',
            'obj_name',
            'trgt_ecef_x_km', 'trgt_ecef_y_km', 'trgt_ecef_z_km',
            'sat_ecef_x_km', 'sat_ecef_y_km', 'sat_ecef_z_km',
            'status'
        ]

        with open(output_path, 'w', newline='') as f:
            writer = csv.writer(f)
            writer.writerow(columns)

            for det in detections:
                time_s = det['time_sec']
                sat_id = det['satellite_id']
                comp_id = det['component_id']

                # Target position
                lat_deg = det['lat_deg']
                lon_deg = det['lon_deg']
                alt_km = det['alt_km']
                alt_m = alt_km * 1000.0

                target_ecef = latlon_to_ecef_wgs84(lat_deg, lon_deg, alt_m)

                # Estimate target velocity from trajectory
                # TAOS trajectory format: col 1,2 are ECEF X,Y in feet, col 3 is ECEF Z in km
                FT_TO_M = 0.3048
                target_vel = np.array([0.0, 0.0, 0.0])
                if comp_id in trajectories:
                    traj = np.array(trajectories[comp_id])
                    times = traj[:, 0]
                    idx = np.argmin(np.abs(times - time_s))

                    if idx > 0 and idx < len(traj) - 1:
                        dt = traj[idx + 1, 0] - traj[idx - 1, 0]
                        if dt > 0:
                            # Convert ECEF from TAOS units to meters
                            pos1 = np.array([
                                traj[idx - 1, 1] * FT_TO_M,
                                traj[idx - 1, 2] * FT_TO_M,
                                traj[idx - 1, 3] * 1000.0  # km to m
                            ])
                            pos2 = np.array([
                                traj[idx + 1, 1] * FT_TO_M,
                                traj[idx + 1, 2] * FT_TO_M,
                                traj[idx + 1, 3] * 1000.0  # km to m
                            ])
                            target_vel = (pos2 - pos1) / dt

                # Satellite position
                sat_info = sat_lookup.get(sat_id, {})
                sat_lat = sat_info.get('lat_deg', 0.0)
                sat_lon = sat_info.get('lon_deg', 0.0)
                sat_alt_km = sat_info.get('altitude_km', 35786.0)
                sat_alt_m = sat_alt_km * 1000.0

                sat_ecef = latlon_to_ecef_wgs84(sat_lat, sat_lon, sat_alt_m)

                # Bearing vector
                bearing = compute_bearing_vector(sat_ecef, target_ecef)

                # Range
                range_km = det.get('range_km', np.linalg.norm(target_ecef - sat_ecef) / 1000.0)

                # LOS status
                in_los = True  # We only have detections when in LOS

                # Sunlit status (above 100 km altitude is typically sunlit)
                sunlit = alt_km > 100.0

                # Solar geometry
                solar_angle, solar_bearing = compute_solar_geometry(time_s, target_ecef)

                # Sensor info
                sensor_name = f"SATELLITE_{sat_id[-5:]}" if len(sat_id) > 5 else f"SATELLITE_{sat_id}"
                platform_name = sat_info.get('platform_name', f"10x10_{sat_id[-4:]}" if len(sat_id) > 4 else sat_id)

                # IR data from RAPTIR
                ir_emitted = det.get('ir_emitted_W_per_sr', 0.0)
                ir_observed = det.get('ir_observed_W_per_sr', 0.0)

                # Radiant intensity (W/sr)
                rad_int = ir_emitted

                # Irradiance at satellite (W/cm^2)
                range_m = range_km * 1000.0
                if range_m > 0:
                    irradiance = ir_emitted / (4 * np.pi * range_m**2) * 1e-4  # Convert to W/cm^2
                else:
                    irradiance = 0.0

                # Scaled irradiance (arbitrary scaling for SNR calculation)
                irradiance_scaled = irradiance * sat_info.get('sensitivity_factor', 1.0)

                # SNR calculation
                sensitivity = sat_info.get('sensitivity_W_per_sr', 1e-10)
                snr = ir_observed / sensitivity if sensitivity > 0 else 0.0

                # Sensor parameters
                meas_error = sat_info.get('meas_error', 5e-5)  # radians
                fov_deg = sat_info.get('fov_deg', 180.0)
                sea_deg = sat_info.get('solar_exclusion_angle', 20.0)

                # Object name (missile class)
                obj_name = meta.get('missile', 'UNKNOWN_CLASS')

                # Status (sensor state)
                if time_s < 10.0:
                    status = 'Slewing'
                elif snr > 10.0:
                    status = 'Tracking'
                else:
                    status = 'Searching'

                row = [
                    time_s,
                    target_ecef[0], target_ecef[1], target_ecef[2],
                    target_vel[0], target_vel[1], target_vel[2],
                    lat_deg, lon_deg, alt_km,
                    sat_ecef[0], sat_ecef[1], sat_ecef[2],
                    sat_lat, sat_lon, sat_alt_km,
                    bearing[0], bearing[1], bearing[2],
                    range_km,
                    in_los,
                    sunlit,
                    solar_angle,
                    solar_bearing[0], solar_bearing[1], solar_bearing[2],
                    sensor_name,
                    platform_name,
                    rad_int,
                    irradiance,
                    irradiance_scaled,
                    snr,
                    meas_error,
                    fov_deg,
                    sea_deg,
                    obj_name,
                    target_ecef[0] / 1000.0, target_ecef[1] / 1000.0, target_ecef[2] / 1000.0,
                    sat_ecef[0] / 1000.0, sat_ecef[1] / 1000.0, sat_ecef[2] / 1000.0,
                    status
                ]

                writer.writerow(row)

        return output_path

    def write_track_data_csv(self,
                              track_data: TrackData,
                              scenario_name: str,
                              arm_mode: bool = False) -> List[Path]:
        """
        Write RIPITT-format TrackData CSV files for each Monte Carlo run.

        Format: Time, pos_ecef_x/y/z, vel_ecef_x/y/z, acc_ecef_x/y/z, LT0-LT44

        Args:
            track_data: TrackData from tracking filter
            scenario_name: Base name for output files
            arm_mode: If True, add _ARM suffix

        Returns:
            List of paths to written files
        """
        basename = scenario_name
        if arm_mode:
            basename = basename + '_ARM'
        basename = basename + '_TrackData'

        output_paths = []

        # Lower triangle indices for 9x9 matrix
        LT_cov_idx_full = np.column_stack([
            np.arange(81),
            np.repeat(np.arange(9), 9),
            np.tile(np.arange(9), 9)
        ])
        LT_cov_idx = LT_cov_idx_full[LT_cov_idx_full[:, 1] >= LT_cov_idx_full[:, 2]][:, 0]

        for mc_num in range(track_data.num_monte):
            # Extract covariance as flat 45-element array
            LT_cov = track_data.LT_covs[mc_num].reshape(
                (track_data.num_time_points, 81))[:, LT_cov_idx]

            # Build data array
            data = np.column_stack([
                track_data.times,
                track_data.positions_ecef[mc_num],
                track_data.velocities_ecef[mc_num],
                track_data.accelerations_ecef[mc_num],
                LT_cov
            ])

            # Column names
            cols = [
                'Time',
                'pos_ecef_x', 'pos_ecef_y', 'pos_ecef_z',
                'vel_ecef_x', 'vel_ecef_y', 'vel_ecef_z',
                'acc_ecef_x', 'acc_ecef_y', 'acc_ecef_z'
            ]
            cols.extend([f'LT{i}' for i in range(45)])

            # Write CSV
            file_name = f"{basename}_MC{mc_num + 1}{extsep}csv"
            output_path = self.output_dir / file_name

            with open(output_path, 'w', newline='') as f:
                writer = csv.writer(f)
                writer.writerow(cols)
                for row in data:
                    writer.writerow(row)

            output_paths.append(output_path)

        return output_paths

    def write_track_metrics_csv(self,
                                 track_metrics: TrackMetrics,
                                 scenario_name: str,
                                 arm_mode: bool = False) -> List[Path]:
        """
        Write RIPITT-format TrackMetrics CSV files for each Monte Carlo run.

        Format: Time, pos_err_x/y/z, vel_err_x/y/z, acc_err_x/y/z, LT0-LT44

        Args:
            track_metrics: TrackMetrics from tracking analysis
            scenario_name: Base name for output files
            arm_mode: If True, add _ARM suffix

        Returns:
            List of paths to written files
        """
        basename = scenario_name
        if arm_mode:
            basename = basename + '_ARM'
        basename = basename + '_TrackMetrics'

        output_paths = []

        # Lower triangle indices
        LT_cov_idx_full = np.column_stack([
            np.arange(81),
            np.repeat(np.arange(9), 9),
            np.tile(np.arange(9), 9)
        ])
        LT_cov_idx = LT_cov_idx_full[LT_cov_idx_full[:, 1] >= LT_cov_idx_full[:, 2]][:, 0]

        for mc_num in range(track_metrics.num_monte):
            # Extract covariance
            LT_cov = track_metrics.LT_covs[mc_num].reshape(
                (track_metrics.num_time_points, 81))[:, LT_cov_idx]

            # Build data array
            data = np.column_stack([
                track_metrics.track_times,
                track_metrics.pos_errors_m[mc_num],
                track_metrics.vel_errors_mps[mc_num],
                track_metrics.acc_errors_mps2[mc_num],
                LT_cov
            ])

            # Column names
            cols = [
                'Time',
                'pos_err_x', 'pos_err_y', 'pos_err_z',
                'vel_err_x', 'vel_err_y', 'vel_err_z',
                'acc_err_x', 'acc_err_y', 'acc_err_z'
            ]
            cols.extend([f'LT{i}' for i in range(45)])

            # Write CSV
            file_name = f"{basename}_MC{mc_num + 1}{extsep}csv"
            output_path = self.output_dir / file_name

            with open(output_path, 'w', newline='') as f:
                writer = csv.writer(f)
                writer.writerow(cols)
                for row in data:
                    writer.writerow(row)

            output_paths.append(output_path)

        return output_paths

    def write_trajectory_csv(self,
                              taos_result: Dict[str, Any],
                              filename: str = None) -> Path:
        """
        Write main trajectory CSV matching original RIPITT format.

        This is the large CSV file with full trajectory data at each timestep.

        Args:
            taos_result: Output from TAOS
            filename: Output filename

        Returns:
            Path to written file
        """
        meta = taos_result.get('meta', {})
        trajectories = taos_result.get('trajectories', {})

        if filename is None:
            obj_name = meta.get('missile_class', 'UNKNOWN_CLASS')
            filename = f"{obj_name}.csv"

        output_path = self.output_dir / filename

        # Get first trajectory
        if not trajectories:
            return output_path

        comp_id = list(trajectories.keys())[0]
        traj = np.array(trajectories[comp_id])

        # Build trajectory data with ECEF conversion
        rows = []
        prev_pos = None
        prev_vel = None
        prev_time = None

        # TAOS trajectory format: col 1,2 are ECEF X,Y in feet, col 3 is ECEF Z in km
        FT_TO_M = 0.3048

        for i, point in enumerate(traj):
            time_s = point[0]
            # TAOS outputs ECEF, not lat/lon/alt
            ecef_x_m = point[1] * FT_TO_M
            ecef_y_m = point[2] * FT_TO_M
            ecef_z_m = point[3] * 1000.0  # km to m

            ecef_pos = np.array([ecef_x_m, ecef_y_m, ecef_z_m])

            # Convert to geodetic for output
            lat_deg, lon_deg, alt_m = ecef_to_geodetic_wgs84(ecef_x_m, ecef_y_m, ecef_z_m)

            # Velocity from finite difference
            if prev_pos is not None and prev_time is not None:
                dt = time_s - prev_time
                if dt > 0:
                    ecef_vel = (ecef_pos - prev_pos) / dt
                else:
                    ecef_vel = np.zeros(3)
            else:
                ecef_vel = np.zeros(3)

            # Acceleration from velocity difference
            if prev_vel is not None and prev_time is not None:
                dt = time_s - prev_time
                if dt > 0:
                    ecef_acc = (ecef_vel - prev_vel) / dt
                else:
                    ecef_acc = np.zeros(3)
            else:
                ecef_acc = np.zeros(3)

            rows.append([
                time_s,
                ecef_pos[0], ecef_pos[1], ecef_pos[2],
                ecef_vel[0], ecef_vel[1], ecef_vel[2],
                ecef_acc[0], ecef_acc[1], ecef_acc[2],
                lat_deg, lon_deg, alt_m / 1000.0,  # alt in km
                np.linalg.norm(ecef_vel)  # velocity magnitude
            ])

            prev_pos = ecef_pos
            prev_vel = ecef_vel
            prev_time = time_s

        # Column names
        columns = [
            'Time',
            'Xecfc', 'Yecfc', 'Zecfc',
            'Xecfcdt', 'Yecfcdt', 'Zecfcdt',
            'Xecfcdt2', 'Yecfcdt2', 'Zecfcdt2',
            'latgd', 'long', 'alt_km', 'vel_mag'
        ]

        with open(output_path, 'w', newline='') as f:
            writer = csv.writer(f)
            writer.writerow(columns)
            for row in rows:
                writer.writerow(row)

        return output_path

    def write_scenario_log(self,
                           scenario_name: str,
                           meta: Dict[str, Any],
                           timing: Dict[str, float] = None) -> Path:
        """Write scenario execution log."""
        import datetime

        output_path = self.output_dir / f"{scenario_name}.log"

        with open(output_path, 'w') as f:
            f.write(f"Simulated Start Time: {datetime.datetime.now().strftime('%Y-%m-%d %H:%M:%S UTC')}\n")
            f.write(f"Launch Delay (min): 0\n")
            f.write(f"Test Option: {meta}\n")
            f.write(f"Threat Trajectory: {meta.get('missile_class', 'UNKNOWN')}\n")
            f.write(f"Results Output File: {scenario_name}\n")

            if timing:
                total_time = sum(timing.values())
                f.write(f"Run Execution Time: {total_time:.3f} sec\n")

            f.write(f"Run Complete Time: {datetime.datetime.now().strftime('%m/%d/%Y %H:%M:%S')}\n")

        return output_path

    def write_all_ripitt_outputs(self,
                                  taos_result: Dict[str, Any],
                                  raptir_result: Dict[str, Any],
                                  hoss_result: Dict[str, Any],
                                  atom_result: Dict[str, Any] = None,
                                  scenario_name: str = None,
                                  num_monte: int = 1,
                                  include_arm: bool = True) -> Dict[str, List[Path]]:
        """
        Generate all RIPITT-compatible output files for a scenario.

        Args:
            taos_result: Output from TAOS
            raptir_result: Output from RAPTIR
            hoss_result: Output from HOSS
            atom_result: Output from ATOM (optional)
            scenario_name: Base name for files
            num_monte: Number of Monte Carlo runs
            include_arm: Generate ARM (cued) versions too

        Returns:
            Dictionary of output file paths by type
        """
        meta = taos_result.get('meta', {})

        if scenario_name is None:
            missile_class = meta.get('missile_class', 'UNKNOWN_CLASS')
            launch_lat = meta.get('launch_lat', 0)
            launch_lon = meta.get('launch_lon', 0)
            target_lat = meta.get('target_lat', 0)
            target_lon = meta.get('target_lon', 0)
            scenario_name = f"{missile_class}_{launch_lat}_{launch_lon}_{target_lat}_{target_lon}"

        outputs = {
            'trajectory': [],
            'sensors': [],
            'track_data': [],
            'track_metrics': [],
            'logs': []
        }

        # 1. Write main trajectory CSV
        traj_path = self.write_trajectory_csv(taos_result, f"{scenario_name}.csv")
        outputs['trajectory'].append(traj_path)

        # 2. Write sensors CSV (non-ARM)
        sensors_path = self.write_sensors_csv(
            taos_result, hoss_result, raptir_result,
            f"{scenario_name}_sensors.csv", arm_mode=False
        )
        outputs['sensors'].append(sensors_path)

        # 3. Write sensors CSV (ARM mode)
        if include_arm:
            sensors_arm_path = self.write_sensors_csv(
                taos_result, hoss_result, raptir_result,
                f"{scenario_name}_sensors_ARM_applied.csv", arm_mode=True
            )
            outputs['sensors'].append(sensors_arm_path)

        # 4. Run tracking and generate TrackData/TrackMetrics
        detections = hoss_result.get('detections', [])
        trajectories = taos_result.get('trajectories', {})

        if detections and trajectories:
            # Get first trajectory as truth
            comp_id = list(trajectories.keys())[0]
            truth_traj = np.array(trajectories[comp_id])

            # Augment detections with satellite info
            satellites = hoss_result.get('satellites', [])
            sat_lookup = {sat['id']: sat for sat in satellites}

            augmented_detections = []
            for det in detections:
                aug_det = det.copy()
                sat_info = sat_lookup.get(det['satellite_id'], {})
                aug_det['sat_lat_deg'] = sat_info.get('lat_deg', 0)
                aug_det['sat_lon_deg'] = sat_info.get('lon_deg', 0)
                aug_det['sat_alt_km'] = sat_info.get('altitude_km', 35786)
                aug_det['meas_error'] = sat_info.get('meas_error', 5e-5)
                augmented_detections.append(aug_det)

            # Run Monte Carlo tracking
            track_data, track_metrics = run_tracking_monte_carlo(
                augmented_detections,
                truth_traj,
                num_monte=num_monte
            )

            # Write TrackData CSVs
            track_data_paths = self.write_track_data_csv(
                track_data, scenario_name, arm_mode=False
            )
            outputs['track_data'].extend(track_data_paths)

            # Write TrackMetrics CSVs
            track_metrics_paths = self.write_track_metrics_csv(
                track_metrics, scenario_name, arm_mode=False
            )
            outputs['track_metrics'].extend(track_metrics_paths)

            # ARM versions
            if include_arm:
                arm_track_data_paths = self.write_track_data_csv(
                    track_data, scenario_name, arm_mode=True
                )
                outputs['track_data'].extend(arm_track_data_paths)

                arm_track_metrics_paths = self.write_track_metrics_csv(
                    track_metrics, scenario_name, arm_mode=True
                )
                outputs['track_metrics'].extend(arm_track_metrics_paths)

        # 5. Write scenario log
        log_path = self.write_scenario_log(scenario_name, meta)
        outputs['logs'].append(log_path)

        return outputs


def export_scenario(scenario_path: str,
                    output_dir: str = None,
                    num_monte: int = 1) -> Dict[str, List[Path]]:
    """
    Export a JEMINI scenario to RIPITT-compatible format.

    Args:
        scenario_path: Path to .pkl.gz scenario file
        output_dir: Output directory (default: same as scenario)
        num_monte: Number of Monte Carlo runs

    Returns:
        Dictionary of output file paths
    """
    import gzip
    import pickle

    scenario_path = Path(scenario_path)

    with gzip.open(scenario_path, 'rb') as f:
        scenario = pickle.load(f)

    if output_dir is None:
        output_dir = scenario_path.parent

    handler = OutputHandler(output_dir)

    # Extract results from scenario
    taos_result = scenario.get('taos', [])
    if isinstance(taos_result, list):
        taos_result = taos_result[0] if taos_result else {}

    raptir_result = scenario.get('raptir', [])
    if isinstance(raptir_result, list):
        raptir_result = raptir_result[0] if raptir_result else {}

    hoss_result = scenario.get('hoss', [])
    if isinstance(hoss_result, list):
        hoss_result = hoss_result[0] if hoss_result else {}

    atom_result = scenario.get('atom', None)

    return handler.write_all_ripitt_outputs(
        taos_result, raptir_result, hoss_result, atom_result,
        num_monte=num_monte
    )


    # =========================================================================
    # BACKWARD COMPATIBILITY METHODS
    # These methods maintain compatibility with existing CLI code
    # =========================================================================

    def write_trajectory_bmrd(self, taos_result: Dict[str, Any],
                               component_id: str = None,
                               filename: str = None) -> Path:
        """Write trajectory in BMRD format (backward compat)."""
        return self.write_trajectory_csv(taos_result, filename)

    def write_measurement_csv(self, taos_result: Dict[str, Any],
                               hoss_result: Dict[str, Any],
                               filename: str = None) -> Path:
        """Write measurement CSV (backward compat - maps to sensors CSV)."""
        return self.write_sensors_csv(taos_result, hoss_result, None, filename, arm_mode=False)

    def write_raptir_signatures_csv(self, raptir_result: Dict[str, Any],
                                     filename: str = None) -> Path:
        """Write RAPTIR IR signatures (backward compat)."""
        ir_signatures = raptir_result.get('ir_signatures', {})
        meta = raptir_result.get('meta', {})

        if filename is None:
            missile_class = meta.get('missile_class', 'UNKNOWN_CLASS')
            filename = f"{missile_class}_RAPTIR_signatures.csv"

        output_path = self.output_dir / filename

        columns = ['time_sec', 'component_id', 'altitude_km', 'temperature_K', 'intensity_W_sr', 'phase']

        with open(output_path, 'w', newline='') as f:
            writer = csv.writer(f)
            writer.writerow(columns)

            for comp_id, sig_data in ir_signatures.items():
                for entry in sig_data:
                    time_s = entry[0]
                    temp_k = entry[1] if len(entry) > 1 else 0.0
                    intensity = entry[2] if len(entry) > 2 else 0.0

                    if intensity > 1000:
                        phase = 'boost'
                    elif intensity > 10:
                        phase = 'post-boost'
                    elif intensity > 0.1:
                        phase = 'midcourse'
                    else:
                        phase = 'terminal'

                    trajectories = raptir_result.get('trajectories', {})
                    alt_km = 0.0
                    if comp_id in trajectories:
                        traj = trajectories[comp_id]
                        for t_entry in traj:
                            if abs(t_entry[0] - time_s) < 0.5:
                                alt_km = t_entry[3] / 1000.0
                                break

                    row = [time_s, comp_id, alt_km, temp_k, intensity, phase]
                    writer.writerow(row)

        return output_path

    def write_darts_measurements_csv(self, taos_result: Dict[str, Any],
                                      darts_result: Dict[str, Any],
                                      filename: str = None) -> Path:
        """Write DARTS radar measurements (backward compat)."""
        detections = darts_result.get('detections', [])
        radars = darts_result.get('radars', [])
        meta = darts_result.get('meta', {})

        if filename is None:
            missile_class = meta.get('missile', 'UNKNOWN_CLASS')
            filename = f"{missile_class}_DARTS_measurements.csv"

        output_path = self.output_dir / filename

        radar_lookup = {r['id']: r for r in radars} if radars else {}

        columns = [
            'time_sec', 'component_id', 'radar_id',
            'range_km', 'azimuth_deg', 'elevation_deg',
            'rcs_dbsm', 'snr_db', 'doppler_mps',
            'target_ecef_x', 'target_ecef_y', 'target_ecef_z',
            'target_lat_deg', 'target_lon_deg', 'target_alt_km',
            'radar_ecef_x', 'radar_ecef_y', 'radar_ecef_z',
            'radar_lat_deg', 'radar_lon_deg'
        ]

        with open(output_path, 'w', newline='') as f:
            writer = csv.writer(f)
            writer.writerow(columns)

            for det in detections:
                time_s = det.get('time_sec', 0.0)
                comp_id = det.get('component_id', '')
                radar_id = det.get('radar_id', '')

                lat_deg = det.get('lat_deg', 0.0)
                lon_deg = det.get('lon_deg', 0.0)
                alt_km = det.get('alt_km', 0.0)
                target_ecef = latlon_to_ecef_wgs84(lat_deg, lon_deg, alt_km * 1000.0)

                radar_info = radar_lookup.get(radar_id, {})
                radar_lat = radar_info.get('lat_deg', 0.0)
                radar_lon = radar_info.get('lon_deg', 0.0)
                radar_ecef = latlon_to_ecef_wgs84(radar_lat, radar_lon, 0.0)

                range_km = det.get('range_km', 0.0)
                rcs_dbsm = det.get('rcs_dbsm', 0.0)
                snr_db = det.get('snr_db', 0.0)

                delta = target_ecef - radar_ecef
                range_m = np.linalg.norm(delta)
                if range_m > 0:
                    lat_r = np.radians(radar_lat)
                    lon_r = np.radians(radar_lon)
                    e = -np.sin(lon_r) * delta[0] + np.cos(lon_r) * delta[1]
                    n = -np.sin(lat_r) * np.cos(lon_r) * delta[0] - np.sin(lat_r) * np.sin(lon_r) * delta[1] + np.cos(lat_r) * delta[2]
                    u = np.cos(lat_r) * np.cos(lon_r) * delta[0] + np.cos(lat_r) * np.sin(lon_r) * delta[1] + np.sin(lat_r) * delta[2]
                    azimuth_deg = np.degrees(np.arctan2(e, n))
                    horiz_range = np.sqrt(e**2 + n**2)
                    elevation_deg = np.degrees(np.arctan2(u, horiz_range))
                else:
                    azimuth_deg = 0.0
                    elevation_deg = 0.0

                doppler_mps = det.get('doppler_mps', 0.0)

                row = [
                    time_s, comp_id, radar_id,
                    range_km, azimuth_deg, elevation_deg,
                    rcs_dbsm, snr_db, doppler_mps,
                    target_ecef[0], target_ecef[1], target_ecef[2],
                    lat_deg, lon_deg, alt_km,
                    radar_ecef[0], radar_ecef[1], radar_ecef[2],
                    radar_lat, radar_lon
                ]
                writer.writerow(row)

        return output_path

    def write_track_csv(self, atom_result: Dict[str, Any],
                        filename: str = None) -> Path:
        """Write ATOM track CSV (backward compat - simplified format)."""
        tracks = atom_result.get('fused_tracks', atom_result.get('tracks', []))
        meta = atom_result.get('meta', {})

        if filename is None:
            missile_class = meta.get('missile', 'UNKNOWN_CLASS')
            filename = f"{missile_class}_TrackData.csv"

        output_path = self.output_dir / filename

        columns = [
            'Time', 'track_id', 'component_id',
            'pos_ecef_x', 'pos_ecef_y', 'pos_ecef_z',
            'vel_ecef_x', 'vel_ecef_y', 'vel_ecef_z',
            'acc_ecef_x', 'acc_ecef_y', 'acc_ecef_z'
        ]
        columns.extend([f'LT{i}' for i in range(45)])

        with open(output_path, 'w', newline='') as f:
            writer = csv.writer(f)
            writer.writerow(columns)

            for track in tracks:
                track_id = track.get('track_id', 0)
                component_id = track.get('component_id', '')
                state_history = track.get('state_history', [])

                if not state_history:
                    final_state = track.get('final_state', {})
                    if final_state:
                        state_history = [{
                            'time_sec': track.get('last_update_time_sec', 0.0),
                            **final_state
                        }]

                prev_pos = None
                prev_vel = None
                prev_time = None

                for state in state_history:
                    time_s = state.get('time_sec', 0.0)
                    lat = state.get('lat_deg', 0.0)
                    lon = state.get('lon_deg', 0.0)
                    alt_km = state.get('alt_km', 0.0)

                    pos_ecef = latlon_to_ecef_wgs84(lat, lon, alt_km * 1000.0)

                    if prev_pos is not None and prev_time is not None:
                        dt = time_s - prev_time
                        if dt > 0:
                            vel_ecef = (pos_ecef - prev_pos) / dt
                        else:
                            vel_ecef = np.zeros(3)
                    else:
                        vel_ecef = np.zeros(3)

                    if prev_vel is not None and prev_time is not None:
                        dt = time_s - prev_time
                        if dt > 0:
                            acc_ecef = (vel_ecef - prev_vel) / dt
                        else:
                            acc_ecef = np.zeros(3)
                    else:
                        acc_ecef = np.zeros(3)

                    # Default covariance (9x9 diagonal)
                    cov = np.diag([
                        1000.0, 1000.0, 1000.0,
                        100.0, 100.0, 100.0,
                        10.0, 10.0, 10.0
                    ])
                    lt_elements = []
                    for i in range(9):
                        for j in range(i + 1):
                            lt_elements.append(cov[i, j])

                    row = [
                        time_s, track_id, component_id,
                        pos_ecef[0], pos_ecef[1], pos_ecef[2],
                        vel_ecef[0], vel_ecef[1], vel_ecef[2],
                        acc_ecef[0], acc_ecef[1], acc_ecef[2]
                    ]
                    row.extend(lt_elements)
                    writer.writerow(row)

                    prev_pos = pos_ecef
                    prev_vel = vel_ecef
                    prev_time = time_s

        return output_path

    def write_cue_log_csv(self, hoss_result: Dict[str, Any],
                          darts_result: Dict[str, Any],
                          atom_result: Dict[str, Any],
                          filename: str = None) -> Path:
        """Write sensor cue log (backward compat)."""
        meta = hoss_result.get('meta', {}) if hoss_result else {}

        if filename is None:
            missile_class = meta.get('missile', 'UNKNOWN_CLASS')
            filename = f"{missile_class}_cue_log.csv"

        output_path = self.output_dir / filename

        columns = [
            'time_sec', 'sensor_type', 'sensor_id', 'component_id',
            'event_type', 'lat_deg', 'lon_deg', 'alt_km',
            'intensity_or_snr', 'range_km'
        ]

        events = []

        if hoss_result:
            for det in hoss_result.get('detections', []):
                events.append({
                    'time_sec': det.get('time_sec', 0.0),
                    'sensor_type': 'IR',
                    'sensor_id': det.get('satellite_id', ''),
                    'component_id': det.get('component_id', ''),
                    'event_type': 'detection',
                    'lat_deg': det.get('lat_deg', 0.0),
                    'lon_deg': det.get('lon_deg', 0.0),
                    'alt_km': det.get('alt_km', 0.0),
                    'intensity_or_snr': det.get('ir_observed_W_per_sr', 0.0),
                    'range_km': det.get('range_km', 0.0)
                })

        if darts_result:
            for det in darts_result.get('detections', []):
                events.append({
                    'time_sec': det.get('time_sec', 0.0),
                    'sensor_type': 'RADAR',
                    'sensor_id': det.get('radar_id', ''),
                    'component_id': det.get('component_id', ''),
                    'event_type': 'detection',
                    'lat_deg': det.get('lat_deg', 0.0),
                    'lon_deg': det.get('lon_deg', 0.0),
                    'alt_km': det.get('alt_km', 0.0),
                    'intensity_or_snr': det.get('snr_db', 0.0),
                    'range_km': det.get('range_km', 0.0)
                })

        if atom_result:
            tracks = atom_result.get('fused_tracks', atom_result.get('tracks', []))
            for track in tracks:
                events.append({
                    'time_sec': track.get('birth_time_sec', 0.0),
                    'sensor_type': 'FUSED',
                    'sensor_id': f"track_{track.get('track_id', 0)}",
                    'component_id': track.get('component_id', ''),
                    'event_type': 'track_birth',
                    'lat_deg': 0.0,
                    'lon_deg': 0.0,
                    'alt_km': 0.0,
                    'intensity_or_snr': track.get('num_detections', 0),
                    'range_km': 0.0
                })

        events.sort(key=lambda x: x['time_sec'])

        with open(output_path, 'w', newline='') as f:
            writer = csv.writer(f)
            writer.writerow(columns)

            for evt in events:
                row = [
                    evt['time_sec'],
                    evt['sensor_type'],
                    evt['sensor_id'],
                    evt['component_id'],
                    evt['event_type'],
                    evt['lat_deg'],
                    evt['lon_deg'],
                    evt['alt_km'],
                    evt['intensity_or_snr'],
                    evt['range_km']
                ]
                writer.writerow(row)

        return output_path

    def write_track_metrics_csv_compat(self, atom_result: Dict[str, Any],
                                        truth_trajectories: Dict[str, Any] = None,
                                        filename: str = None) -> Path:
        """Write track metrics (backward compat version for dict input)."""
        tracks = atom_result.get('fused_tracks', atom_result.get('tracks', []))
        meta = atom_result.get('meta', {})

        if filename is None:
            missile_class = meta.get('missile', 'UNKNOWN_CLASS')
            filename = f"{missile_class}_TrackMetrics.csv"

        output_path = self.output_dir / filename

        columns = [
            'Time', 'track_id', 'component_id',
            'pos_err_x', 'pos_err_y', 'pos_err_z',
            'vel_err_x', 'vel_err_y', 'vel_err_z',
            'acc_err_x', 'acc_err_y', 'acc_err_z',
            'pos_err_mag_m', 'vel_err_mag_mps'
        ]
        columns.extend([f'LT{i}' for i in range(45)])

        with open(output_path, 'w', newline='') as f:
            writer = csv.writer(f)
            writer.writerow(columns)

            for track in tracks:
                track_id = track.get('track_id', 0)
                component_id = track.get('component_id', '')
                state_history = track.get('state_history', [])

                truth = None
                if truth_trajectories and component_id in truth_trajectories:
                    truth = np.array(truth_trajectories[component_id])

                for state in state_history:
                    time_s = state.get('time_sec', 0.0)
                    lat = state.get('lat_deg', 0.0)
                    lon = state.get('lon_deg', 0.0)
                    alt_km = state.get('alt_km', 0.0)
                    est_pos = latlon_to_ecef_wgs84(lat, lon, alt_km * 1000.0)
                    est_vel = np.array([0.0, 0.0, 0.0])

                    if truth is not None:
                        times = truth[:, 0]
                        idx = np.argmin(np.abs(times - time_s))
                        true_lat = truth[idx, 1]
                        true_lon = truth[idx, 2]
                        true_alt_m = truth[idx, 3]
                        true_pos = latlon_to_ecef_wgs84(true_lat, true_lon, true_alt_m)
                        pos_err = est_pos - true_pos
                        pos_err_mag = np.linalg.norm(pos_err)
                        vel_err = np.array([0.0, 0.0, 0.0])
                        vel_err_mag = 0.0
                    else:
                        pos_err = np.array([0.0, 0.0, 0.0])
                        pos_err_mag = track.get('final_position_uncertainty_km', 0.0) * 1000.0
                        vel_err = np.array([0.0, 0.0, 0.0])
                        vel_err_mag = 0.0

                    acc_err = np.array([0.0, 0.0, 0.0])

                    cov = np.diag([
                        1000.0, 1000.0, 1000.0,
                        100.0, 100.0, 100.0,
                        10.0, 10.0, 10.0
                    ])
                    lt_elements = []
                    for i in range(9):
                        for j in range(i + 1):
                            lt_elements.append(cov[i, j])

                    row = [
                        time_s, track_id, component_id,
                        pos_err[0], pos_err[1], pos_err[2],
                        vel_err[0], vel_err[1], vel_err[2],
                        acc_err[0], acc_err[1], acc_err[2],
                        pos_err_mag, vel_err_mag
                    ]
                    row.extend(lt_elements)
                    writer.writerow(row)

        return output_path


def export_scenario(scenario_path: str,
                    output_dir: str = None,
                    num_monte: int = 1) -> Dict[str, List[Path]]:
    """
    Export a JEMINI scenario to RIPITT-compatible format.

    Args:
        scenario_path: Path to .pkl.gz scenario file
        output_dir: Output directory (default: same as scenario)
        num_monte: Number of Monte Carlo runs

    Returns:
        Dictionary of output file paths
    """
    import gzip
    import pickle

    scenario_path = Path(scenario_path)

    with gzip.open(scenario_path, 'rb') as f:
        scenario = pickle.load(f)

    if output_dir is None:
        output_dir = scenario_path.parent

    handler = OutputHandler(output_dir)

    # Extract results from scenario
    taos_result = scenario.get('taos', [])
    if isinstance(taos_result, list):
        taos_result = taos_result[0] if taos_result else {}

    raptir_result = scenario.get('raptir', [])
    if isinstance(raptir_result, list):
        raptir_result = raptir_result[0] if raptir_result else {}

    hoss_result = scenario.get('hoss', [])
    if isinstance(hoss_result, list):
        hoss_result = hoss_result[0] if hoss_result else {}

    atom_result = scenario.get('atom', None)

    return handler.write_all_ripitt_outputs(
        taos_result, raptir_result, hoss_result, atom_result,
        num_monte=num_monte
    )


__all__ = [
    'OutputHandler',
    'export_scenario',
    'latlon_to_ecef_wgs84',
    'compute_bearing_vector',
    'compute_solar_geometry'
]
