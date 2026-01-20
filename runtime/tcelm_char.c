/*
 * tcelm_char.c - Elm Char module implementation
 */

#include "tcelm_char.h"
#include <ctype.h>

/*
 * Char.isUpper : Char -> Bool
 */
tcelm_value_t *tcelm_char_isUpper(tcelm_arena_t *arena, tcelm_value_t *c) {
    (void)arena;
    uint32_t cp = TCELM_AS_CHAR(c);
    /* Simple ASCII check - full Unicode would need more */
    return (cp >= 'A' && cp <= 'Z') ? TCELM_TRUE : TCELM_FALSE;
}

/*
 * Char.isLower : Char -> Bool
 */
tcelm_value_t *tcelm_char_isLower(tcelm_arena_t *arena, tcelm_value_t *c) {
    (void)arena;
    uint32_t cp = TCELM_AS_CHAR(c);
    return (cp >= 'a' && cp <= 'z') ? TCELM_TRUE : TCELM_FALSE;
}

/*
 * Char.isAlpha : Char -> Bool
 */
tcelm_value_t *tcelm_char_isAlpha(tcelm_arena_t *arena, tcelm_value_t *c) {
    (void)arena;
    uint32_t cp = TCELM_AS_CHAR(c);
    return ((cp >= 'A' && cp <= 'Z') || (cp >= 'a' && cp <= 'z')) ? TCELM_TRUE : TCELM_FALSE;
}

/*
 * Char.isAlphaNum : Char -> Bool
 */
tcelm_value_t *tcelm_char_isAlphaNum(tcelm_arena_t *arena, tcelm_value_t *c) {
    (void)arena;
    uint32_t cp = TCELM_AS_CHAR(c);
    return ((cp >= 'A' && cp <= 'Z') ||
            (cp >= 'a' && cp <= 'z') ||
            (cp >= '0' && cp <= '9')) ? TCELM_TRUE : TCELM_FALSE;
}

/*
 * Char.isDigit : Char -> Bool
 */
tcelm_value_t *tcelm_char_isDigit(tcelm_arena_t *arena, tcelm_value_t *c) {
    (void)arena;
    uint32_t cp = TCELM_AS_CHAR(c);
    return (cp >= '0' && cp <= '9') ? TCELM_TRUE : TCELM_FALSE;
}

/*
 * Char.isOctDigit : Char -> Bool
 */
tcelm_value_t *tcelm_char_isOctDigit(tcelm_arena_t *arena, tcelm_value_t *c) {
    (void)arena;
    uint32_t cp = TCELM_AS_CHAR(c);
    return (cp >= '0' && cp <= '7') ? TCELM_TRUE : TCELM_FALSE;
}

/*
 * Char.isHexDigit : Char -> Bool
 */
tcelm_value_t *tcelm_char_isHexDigit(tcelm_arena_t *arena, tcelm_value_t *c) {
    (void)arena;
    uint32_t cp = TCELM_AS_CHAR(c);
    return ((cp >= '0' && cp <= '9') ||
            (cp >= 'a' && cp <= 'f') ||
            (cp >= 'A' && cp <= 'F')) ? TCELM_TRUE : TCELM_FALSE;
}

/*
 * Char.toUpper : Char -> Char
 */
tcelm_value_t *tcelm_char_toUpper(tcelm_arena_t *arena, tcelm_value_t *c) {
    uint32_t cp = TCELM_AS_CHAR(c);
    if (cp >= 'a' && cp <= 'z') {
        cp = cp - 'a' + 'A';
    }
    return tcelm_char(arena, cp);
}

/*
 * Char.toLower : Char -> Char
 */
tcelm_value_t *tcelm_char_toLower(tcelm_arena_t *arena, tcelm_value_t *c) {
    uint32_t cp = TCELM_AS_CHAR(c);
    if (cp >= 'A' && cp <= 'Z') {
        cp = cp - 'A' + 'a';
    }
    return tcelm_char(arena, cp);
}

/*
 * Char.toCode : Char -> Int
 */
tcelm_value_t *tcelm_char_toCode(tcelm_arena_t *arena, tcelm_value_t *c) {
    return tcelm_int(arena, (int64_t)TCELM_AS_CHAR(c));
}

/*
 * Char.fromCode : Int -> Char
 */
tcelm_value_t *tcelm_char_fromCode(tcelm_arena_t *arena, tcelm_value_t *code) {
    return tcelm_char(arena, (uint32_t)TCELM_AS_INT(code));
}
