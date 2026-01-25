/*
 * math.h - Minimal math for tcelm bare-metal
 */

#ifndef _MATH_H
#define _MATH_H

/* Basic math operations - software implementations */
double sqrt(double x);
double sin(double x);
double cos(double x);
double tan(double x);
double floor(double x);
double ceil(double x);
double fabs(double x);
double pow(double x, double y);
double log(double x);
double exp(double x);

/* Constants */
#define M_PI 3.14159265358979323846
#define M_E  2.71828182845904523536

/* NaN and Infinity */
#define NAN (__builtin_nan(""))
#define INFINITY (__builtin_inf())

#endif /* _MATH_H */
