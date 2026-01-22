/*
 * fdlibm_extra.c - Additional math functions not in FDLIBM
 *
 * Provides C99 functions and float versions for TCC-NUC-BSP math.h
 */

#include "fdlibm.h"

/* Constants from fdlibm.h */
#define LN2_HI  6.93147180369123816490e-01   /* 0x3fe62e42, 0xfee00000 */
#define LN2_LO  1.90821492927058770002e-10   /* 0x3dea39ef, 0x35793c76 */
#define LN2     6.93147180559945286227e-01   /* 0x3fe62e42, 0xfefa39ef */
#define INV_LN2 1.44269504088896338700e+00   /* 0x3ff71547, 0x652b82fe */

/*
 * log2(x) - base-2 logarithm
 *
 * log2(x) = log(x) / log(2) = log(x) * INV_LN2
 */
double log2(double x)
{
    return log(x) * INV_LN2;
}

/*
 * round(x) - round to nearest integer (ties to even)
 *
 * Uses rint() which does banker's rounding
 */
double round(double x)
{
    if (x >= 0.0) {
        double f = floor(x);
        double diff = x - f;
        if (diff > 0.5) return f + 1.0;
        if (diff < 0.5) return f;
        /* diff == 0.5: round to nearest even */
        if (fmod(f, 2.0) == 0.0) return f;
        return f + 1.0;
    } else {
        double c = ceil(x);
        double diff = c - x;
        if (diff > 0.5) return c - 1.0;
        if (diff < 0.5) return c;
        /* diff == 0.5: round to nearest even */
        if (fmod(c, 2.0) == 0.0) return c;
        return c - 1.0;
    }
}

/*
 * trunc(x) - truncate to integer toward zero
 */
double trunc(double x)
{
    if (x >= 0.0) {
        return floor(x);
    } else {
        return ceil(x);
    }
}

/*
 * fmin(x, y) - minimum of two values (NaN-safe)
 */
double fmin(double x, double y)
{
    if (isnan(x)) return y;
    if (isnan(y)) return x;
    return (x < y) ? x : y;
}

/*
 * fmax(x, y) - maximum of two values (NaN-safe)
 */
double fmax(double x, double y)
{
    if (isnan(x)) return y;
    if (isnan(y)) return x;
    return (x > y) ? x : y;
}

/*
 * isinf(x) - check if infinite
 */
int isinf(double x)
{
    int hx = __HI(x);
    int lx = __LO(x);
    hx &= 0x7fffffff;  /* Clear sign bit */
    /* Infinity: exponent all 1s, mantissa all 0s */
    if (hx == 0x7ff00000 && lx == 0) {
        return (__HI(x) < 0) ? -1 : 1;
    }
    return 0;
}

/*
 * isfinite(x) - check if finite (not inf or nan)
 */
int isfinite(double x)
{
    int hx = __HI(x);
    hx &= 0x7fffffff;
    /* Finite if exponent < 2047 */
    return (hx < 0x7ff00000);
}

/*
 * Float versions - wrappers around double functions
 */

float sinf(float x)
{
    return (float)sin((double)x);
}

float cosf(float x)
{
    return (float)cos((double)x);
}

float tanf(float x)
{
    return (float)tan((double)x);
}

float sqrtf(float x)
{
    return (float)sqrt((double)x);
}

float powf(float x, float y)
{
    return (float)pow((double)x, (double)y);
}

float fabsf(float x)
{
    return (float)fabs((double)x);
}

float floorf(float x)
{
    return (float)floor((double)x);
}

float ceilf(float x)
{
    return (float)ceil((double)x);
}

float expf(float x)
{
    return (float)exp((double)x);
}

float logf(float x)
{
    return (float)log((double)x);
}

float log10f(float x)
{
    return (float)log10((double)x);
}

float log2f(float x)
{
    return (float)log2((double)x);
}

float asinf(float x)
{
    return (float)asin((double)x);
}

float acosf(float x)
{
    return (float)acos((double)x);
}

float atanf(float x)
{
    return (float)atan((double)x);
}

float atan2f(float y, float x)
{
    return (float)atan2((double)y, (double)x);
}

float sinhf(float x)
{
    return (float)sinh((double)x);
}

float coshf(float x)
{
    return (float)cosh((double)x);
}

float tanhf(float x)
{
    return (float)tanh((double)x);
}

float roundf(float x)
{
    return (float)round((double)x);
}

float truncf(float x)
{
    return (float)trunc((double)x);
}

float fminf(float x, float y)
{
    return (float)fmin((double)x, (double)y);
}

float fmaxf(float x, float y)
{
    return (float)fmax((double)x, (double)y);
}

float fmodf(float x, float y)
{
    return (float)fmod((double)x, (double)y);
}

/*
 * Long double versions (same as double on x86-32 TCC)
 */
long double sinl(long double x) { return sin(x); }
long double cosl(long double x) { return cos(x); }
long double tanl(long double x) { return tan(x); }
long double sqrtl(long double x) { return sqrt(x); }
long double powl(long double x, long double y) { return pow(x, y); }
long double fabsl(long double x) { return fabs(x); }
long double floorl(long double x) { return floor(x); }
long double ceill(long double x) { return ceil(x); }
long double expl(long double x) { return exp(x); }
long double logl(long double x) { return log(x); }
