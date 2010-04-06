#include <GL/gl.h>
#include <GL/glu.h>
#include <GL/glut.h>

/*
 * Richard Mann
 * 18 May 1998
 *
 * Copyright 1998 NEC Research Institute, Inc. All rights reserved.
 *
 */

unsigned glob_glutStrokeRoman( void ) { return (unsigned)&glutStrokeRoman; }
unsigned glob_glutStrokeMonoRoman( void ) { return (unsigned)&glutStrokeMonoRoman; }
unsigned glob_glutBitmap9By15( void ) { return (unsigned)&glutBitmap9By15; }
unsigned glob_glutBitmap8By13( void ) { return (unsigned)&glutBitmap8By13; }
unsigned glob_glutBitmapTimesRoman10( void ) { return (unsigned)&glutBitmapTimesRoman10; }
unsigned glob_glutBitmapTimesRoman24( void ) { return (unsigned)&glutBitmapTimesRoman24; }
unsigned glob_glutBitmapHelvetica10( void ) { return (unsigned)&glutBitmapHelvetica10; }
unsigned glob_glutBitmapHelvetica12( void ) { return (unsigned)&glutBitmapHelvetica12; }
unsigned glob_glutBitmapHelvetica18( void ) { return (unsigned)&glutBitmapHelvetica18; }
