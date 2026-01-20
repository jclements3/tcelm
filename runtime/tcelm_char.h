/*
 * tcelm_char.h - Elm Char module for tcelm runtime
 */

#ifndef TCELM_CHAR_H
#define TCELM_CHAR_H

#include "tcelm_types.h"
#include "tcelm_arena.h"

/* Char predicates */
tcelm_value_t *tcelm_char_isUpper(tcelm_arena_t *arena, tcelm_value_t *c);
tcelm_value_t *tcelm_char_isLower(tcelm_arena_t *arena, tcelm_value_t *c);
tcelm_value_t *tcelm_char_isAlpha(tcelm_arena_t *arena, tcelm_value_t *c);
tcelm_value_t *tcelm_char_isAlphaNum(tcelm_arena_t *arena, tcelm_value_t *c);
tcelm_value_t *tcelm_char_isDigit(tcelm_arena_t *arena, tcelm_value_t *c);
tcelm_value_t *tcelm_char_isOctDigit(tcelm_arena_t *arena, tcelm_value_t *c);
tcelm_value_t *tcelm_char_isHexDigit(tcelm_arena_t *arena, tcelm_value_t *c);

/* Char conversion */
tcelm_value_t *tcelm_char_toUpper(tcelm_arena_t *arena, tcelm_value_t *c);
tcelm_value_t *tcelm_char_toLower(tcelm_arena_t *arena, tcelm_value_t *c);
tcelm_value_t *tcelm_char_toCode(tcelm_arena_t *arena, tcelm_value_t *c);
tcelm_value_t *tcelm_char_fromCode(tcelm_arena_t *arena, tcelm_value_t *code);

#endif /* TCELM_CHAR_H */
