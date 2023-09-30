#ifndef OBX_PAL_API_H
#define OBX_PAL_API_H

/*
* Copyright 2023 Rochus Keller <mailto:me@rochus-keller.ch>
*
* This file is part of the Oberon+ parser/compiler library.
*
* The following is the license that applies to this copy of the
* file. For a license to use the file under conditions
* other than those described here, please email to me@rochus-keller.ch.
*
* This file may be used under the terms of the GNU Lesser
* General Public License version 2.1 or version 3 as published by the Free
* Software Foundation and appearing in the file LICENSE.LGPLv21 and
* LICENSE.LGPLv3 included in the packaging of this file. Please review the
* following information to ensure the GNU Lesser General Public License
* requirements will be met: https://www.gnu.org/licenses/lgpl.html and
* http://www.gnu.org/licenses/old-licenses/lgpl-2.1.html.
*/

#include <stdint.h>

#ifdef _WIN32
#define PAL_EXPORT __declspec(dllexport)
#else
#define PAL_EXPORT
#endif

/**************** General **********************************/

PAL_EXPORT int32_t PAL_time(); // milliseconds
PAL_EXPORT void PAL_dispose();
PAL_EXPORT void PAL_printChar(char ch);

/**************** File **********************************/

PAL_EXPORT int32_t PAL_file_list();
PAL_EXPORT const char* PAL_file_name(int32_t i);
PAL_EXPORT int32_t PAL_file_open(const char* filename);
PAL_EXPORT int32_t PAL_file_key(const char* filename);
PAL_EXPORT int32_t PAL_file_new();
PAL_EXPORT void PAL_file_free(int32_t buffer);
PAL_EXPORT int PAL_file_save(const char* filename, int32_t buffer);
PAL_EXPORT int PAL_file_remove(const char* filename);
PAL_EXPORT int PAL_file_rename(const char* oldName, const char* newName);
PAL_EXPORT int32_t PAL_file_length(int32_t buffer);
PAL_EXPORT int PAL_file_seek(int32_t buffer, int32_t pos);
PAL_EXPORT int32_t PAL_file_pos(int32_t buffer);
PAL_EXPORT int PAL_file_eof(int32_t buffer);
PAL_EXPORT int PAL_file_write_byte(int32_t buffer, int32_t byte_);
PAL_EXPORT int32_t PAL_file_read_byte(int32_t buffer);

/**************** Display **********************************/

enum KeyState { Right = 1, Mid = 2, Left = 4 }; // corresponds to the Oberon System assignment
enum ShiftState { SHIFT = 1, CTRL = 2, ALT = 4 }; // corresponds to Oberon System assignment
enum PixelFormat { Mono = 0, Index8 = 1, Color565 = 2, Color888 = 3, Color8888 = 4 };

PAL_EXPORT int PAL_open_screen(int width, int height, int format, const char* title, void* buffer);
PAL_EXPORT int PAL_process_events(int sleep);
PAL_EXPORT int PAL_next_key();
PAL_EXPORT int PAL_pending_keys();
PAL_EXPORT int PAL_mouse_state(int* x, int* y, int* keys);
PAL_EXPORT int PAL_modifier_state(int* modifiers);
PAL_EXPORT int PAL_update( int x, int y, int w, int h);

/**************** Math **********************************/

PAL_EXPORT double PAL_sin(double x);
PAL_EXPORT double PAL_cos(double x);
PAL_EXPORT double PAL_arctan(double x);
PAL_EXPORT double PAL_sqrt(double x);
PAL_EXPORT double PAL_ln(double x);
PAL_EXPORT double PAL_exp(double x);
PAL_EXPORT double PAL_pow(double x, double e);


#endif // OBX_PAL_API_H
