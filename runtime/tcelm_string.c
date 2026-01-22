/*
 * tcelm_string.c - Elm String module implementation
 */

#include "tcelm_string.h"
#include <string.h>
#include <stdio.h>
#include <ctype.h>

/* Helper: get byte offset for character index in UTF-8 string */
static size_t utf8_char_to_byte_offset(const char *s, size_t len, int64_t char_index) {
    if (char_index <= 0) return 0;

    size_t byte_offset = 0;
    int64_t char_count = 0;

    while (byte_offset < len && char_count < char_index) {
        unsigned char c = (unsigned char)s[byte_offset];
        if (c < 0x80) {
            byte_offset += 1;
        } else if ((c & 0xE0) == 0xC0) {
            byte_offset += 2;
        } else if ((c & 0xF0) == 0xE0) {
            byte_offset += 3;
        } else {
            byte_offset += 4;
        }
        char_count++;
    }

    return byte_offset;
}

/* Helper: decode one UTF-8 character, return codepoint and advance pointer */
static uint32_t utf8_decode(const char **ptr) {
    const unsigned char *s = (const unsigned char *)*ptr;
    uint32_t cp;

    if (s[0] < 0x80) {
        cp = s[0];
        *ptr += 1;
    } else if ((s[0] & 0xE0) == 0xC0) {
        cp = ((s[0] & 0x1F) << 6) | (s[1] & 0x3F);
        *ptr += 2;
    } else if ((s[0] & 0xF0) == 0xE0) {
        cp = ((s[0] & 0x0F) << 12) | ((s[1] & 0x3F) << 6) | (s[2] & 0x3F);
        *ptr += 3;
    } else {
        cp = ((s[0] & 0x07) << 18) | ((s[1] & 0x3F) << 12) | ((s[2] & 0x3F) << 6) | (s[3] & 0x3F);
        *ptr += 4;
    }

    return cp;
}

/* Helper: encode one UTF-8 character, return bytes written */
static size_t utf8_encode(uint32_t cp, char *out) {
    if (cp < 0x80) {
        out[0] = (char)cp;
        return 1;
    } else if (cp < 0x800) {
        out[0] = (char)(0xC0 | (cp >> 6));
        out[1] = (char)(0x80 | (cp & 0x3F));
        return 2;
    } else if (cp < 0x10000) {
        out[0] = (char)(0xE0 | (cp >> 12));
        out[1] = (char)(0x80 | ((cp >> 6) & 0x3F));
        out[2] = (char)(0x80 | (cp & 0x3F));
        return 3;
    } else {
        out[0] = (char)(0xF0 | (cp >> 18));
        out[1] = (char)(0x80 | ((cp >> 12) & 0x3F));
        out[2] = (char)(0x80 | ((cp >> 6) & 0x3F));
        out[3] = (char)(0x80 | (cp & 0x3F));
        return 4;
    }
}

/*
 * String.fromInt : Int -> String
 */
tcelm_value_t *tcelm_string_fromInt(tcelm_arena_t *arena, tcelm_value_t *n) {
    char buf[32];
    int len = snprintf(buf, sizeof(buf), "%lld", (long long)TCELM_AS_INT(n));
    return tcelm_string_len(arena, buf, (size_t)len);
}

/*
 * String.fromFloat : Float -> String
 */
tcelm_value_t *tcelm_string_fromFloat(tcelm_arena_t *arena, tcelm_value_t *f) {
    char buf[64];
    int len = snprintf(buf, sizeof(buf), "%g", TCELM_AS_FLOAT(f));
    return tcelm_string_len(arena, buf, (size_t)len);
}

/*
 * String.fromChar : Char -> String
 */
tcelm_value_t *tcelm_string_fromChar(tcelm_arena_t *arena, tcelm_value_t *c) {
    char buf[5];
    size_t len = utf8_encode(TCELM_AS_CHAR(c), buf);
    return tcelm_string_len(arena, buf, len);
}

/*
 * String.append (++) : String -> String -> String
 */
tcelm_value_t *tcelm_string_append(tcelm_arena_t *arena, tcelm_value_t *a, tcelm_value_t *b) {
    tcelm_string_t *sa = TCELM_AS_STRING(a);
    tcelm_string_t *sb = TCELM_AS_STRING(b);

    size_t total_len = sa->length + sb->length;

    tcelm_value_t *v = tcelm_arena_alloc(arena, sizeof(tcelm_value_t));
    tcelm_string_t *str = tcelm_arena_alloc(arena, sizeof(tcelm_string_t) + total_len + 1);

    str->length = total_len;
    str->char_count = sa->char_count + sb->char_count;
    memcpy(str->data, sa->data, sa->length);
    memcpy(str->data + sa->length, sb->data, sb->length);
    str->data[total_len] = '\0';

    v->tag = TCELM_TAG_STRING;
    v->data.s = str;
    return v;
}

/*
 * String.concat : List String -> String
 */
tcelm_value_t *tcelm_string_concat(tcelm_arena_t *arena, tcelm_value_t *list) {
    /* First pass: calculate total length */
    size_t total_len = 0;
    size_t total_chars = 0;
    tcelm_value_t *curr = list;

    while (!tcelm_is_nil(curr)) {
        tcelm_value_t *head = tcelm_list_head(curr);
        tcelm_string_t *s = TCELM_AS_STRING(head);
        total_len += s->length;
        total_chars += s->char_count;
        curr = tcelm_list_tail(curr);
    }

    /* Allocate result */
    tcelm_value_t *v = tcelm_arena_alloc(arena, sizeof(tcelm_value_t));
    tcelm_string_t *str = tcelm_arena_alloc(arena, sizeof(tcelm_string_t) + total_len + 1);

    str->length = total_len;
    str->char_count = total_chars;

    /* Second pass: copy strings */
    size_t offset = 0;
    curr = list;
    while (!tcelm_is_nil(curr)) {
        tcelm_value_t *head = tcelm_list_head(curr);
        tcelm_string_t *s = TCELM_AS_STRING(head);
        memcpy(str->data + offset, s->data, s->length);
        offset += s->length;
        curr = tcelm_list_tail(curr);
    }
    str->data[total_len] = '\0';

    v->tag = TCELM_TAG_STRING;
    v->data.s = str;
    return v;
}

/*
 * String.join : String -> List String -> String
 */
tcelm_value_t *tcelm_string_join(tcelm_arena_t *arena, tcelm_value_t *sep, tcelm_value_t *list) {
    tcelm_string_t *sep_s = TCELM_AS_STRING(sep);

    /* Count items and calculate total length */
    size_t total_len = 0;
    size_t total_chars = 0;
    size_t count = 0;
    tcelm_value_t *curr = list;

    while (!tcelm_is_nil(curr)) {
        tcelm_value_t *head = tcelm_list_head(curr);
        tcelm_string_t *s = TCELM_AS_STRING(head);
        total_len += s->length;
        total_chars += s->char_count;
        count++;
        curr = tcelm_list_tail(curr);
    }

    if (count == 0) {
        return tcelm_string(arena, "");
    }

    /* Add separator lengths */
    total_len += (count - 1) * sep_s->length;
    total_chars += (count - 1) * sep_s->char_count;

    /* Allocate result */
    tcelm_value_t *v = tcelm_arena_alloc(arena, sizeof(tcelm_value_t));
    tcelm_string_t *str = tcelm_arena_alloc(arena, sizeof(tcelm_string_t) + total_len + 1);

    str->length = total_len;
    str->char_count = total_chars;

    /* Copy strings with separators */
    size_t offset = 0;
    size_t idx = 0;
    curr = list;
    while (!tcelm_is_nil(curr)) {
        if (idx > 0) {
            memcpy(str->data + offset, sep_s->data, sep_s->length);
            offset += sep_s->length;
        }

        tcelm_value_t *head = tcelm_list_head(curr);
        tcelm_string_t *s = TCELM_AS_STRING(head);
        memcpy(str->data + offset, s->data, s->length);
        offset += s->length;

        idx++;
        curr = tcelm_list_tail(curr);
    }
    str->data[total_len] = '\0';

    v->tag = TCELM_TAG_STRING;
    v->data.s = str;
    return v;
}

/*
 * String.length : String -> Int
 */
tcelm_value_t *tcelm_string_length(tcelm_arena_t *arena, tcelm_value_t *s) {
    return tcelm_int(arena, (int64_t)TCELM_AS_STRING(s)->char_count);
}

/*
 * String.slice : Int -> Int -> String -> String
 */
tcelm_value_t *tcelm_string_slice(tcelm_arena_t *arena, tcelm_value_t *start_v,
                                   tcelm_value_t *end_v, tcelm_value_t *s) {
    tcelm_string_t *str = TCELM_AS_STRING(s);
    int64_t start = TCELM_AS_INT(start_v);
    int64_t end = TCELM_AS_INT(end_v);
    int64_t len = (int64_t)str->char_count;

    /* Handle negative indices */
    if (start < 0) start = len + start;
    if (end < 0) end = len + end;

    /* Clamp to bounds */
    if (start < 0) start = 0;
    if (end < 0) end = 0;
    if (start > len) start = len;
    if (end > len) end = len;
    if (start > end) start = end;

    /* Calculate byte offsets */
    size_t byte_start = utf8_char_to_byte_offset(str->data, str->length, start);
    size_t byte_end = utf8_char_to_byte_offset(str->data, str->length, end);

    return tcelm_string_len(arena, str->data + byte_start, byte_end - byte_start);
}

/*
 * String.left : Int -> String -> String
 */
tcelm_value_t *tcelm_string_left(tcelm_arena_t *arena, tcelm_value_t *n, tcelm_value_t *s) {
    return tcelm_string_slice(arena, tcelm_int(arena, 0), n, s);
}

/*
 * String.right : Int -> String -> String
 */
tcelm_value_t *tcelm_string_right(tcelm_arena_t *arena, tcelm_value_t *n, tcelm_value_t *s) {
    tcelm_string_t *str = TCELM_AS_STRING(s);
    int64_t len = (int64_t)str->char_count;
    int64_t count = TCELM_AS_INT(n);
    return tcelm_string_slice(arena, tcelm_int(arena, len - count), tcelm_int(arena, len), s);
}

/*
 * String.dropLeft : Int -> String -> String
 */
tcelm_value_t *tcelm_string_dropLeft(tcelm_arena_t *arena, tcelm_value_t *n, tcelm_value_t *s) {
    tcelm_string_t *str = TCELM_AS_STRING(s);
    int64_t len = (int64_t)str->char_count;
    return tcelm_string_slice(arena, n, tcelm_int(arena, len), s);
}

/*
 * String.dropRight : Int -> String -> String
 */
tcelm_value_t *tcelm_string_dropRight(tcelm_arena_t *arena, tcelm_value_t *n, tcelm_value_t *s) {
    tcelm_string_t *str = TCELM_AS_STRING(s);
    int64_t len = (int64_t)str->char_count;
    int64_t count = TCELM_AS_INT(n);
    return tcelm_string_slice(arena, tcelm_int(arena, 0), tcelm_int(arena, len - count), s);
}

/*
 * String.isEmpty : String -> Bool
 */
tcelm_value_t *tcelm_string_isEmpty(tcelm_arena_t *arena, tcelm_value_t *s) {
    (void)arena;
    return TCELM_AS_STRING(s)->length == 0 ? TCELM_TRUE : TCELM_FALSE;
}

/*
 * String.contains : String -> String -> Bool
 */
tcelm_value_t *tcelm_string_contains(tcelm_arena_t *arena, tcelm_value_t *sub, tcelm_value_t *s) {
    (void)arena;
    tcelm_string_t *sub_s = TCELM_AS_STRING(sub);
    tcelm_string_t *str = TCELM_AS_STRING(s);
    return strstr(str->data, sub_s->data) != NULL ? TCELM_TRUE : TCELM_FALSE;
}

/*
 * String.startsWith : String -> String -> Bool
 */
tcelm_value_t *tcelm_string_startsWith(tcelm_arena_t *arena, tcelm_value_t *prefix, tcelm_value_t *s) {
    (void)arena;
    tcelm_string_t *pre = TCELM_AS_STRING(prefix);
    tcelm_string_t *str = TCELM_AS_STRING(s);
    if (pre->length > str->length) return TCELM_FALSE;
    return strncmp(str->data, pre->data, pre->length) == 0 ? TCELM_TRUE : TCELM_FALSE;
}

/*
 * String.endsWith : String -> String -> Bool
 */
tcelm_value_t *tcelm_string_endsWith(tcelm_arena_t *arena, tcelm_value_t *suffix, tcelm_value_t *s) {
    (void)arena;
    tcelm_string_t *suf = TCELM_AS_STRING(suffix);
    tcelm_string_t *str = TCELM_AS_STRING(s);
    if (suf->length > str->length) return TCELM_FALSE;
    return strcmp(str->data + str->length - suf->length, suf->data) == 0 ? TCELM_TRUE : TCELM_FALSE;
}

/*
 * String.toList : String -> List Char
 */
tcelm_value_t *tcelm_string_toList(tcelm_arena_t *arena, tcelm_value_t *s) {
    tcelm_string_t *str = TCELM_AS_STRING(s);
    tcelm_value_t *result = TCELM_NIL;

    /* Build list in reverse, then we'll need to reverse it */
    const char *ptr = str->data;
    const char *end = str->data + str->length;

    /* First, count characters and collect them */
    uint32_t chars[1024];  /* Reasonable limit for stack allocation */
    size_t count = 0;

    while (ptr < end && count < 1024) {
        chars[count++] = utf8_decode(&ptr);
    }

    /* Build list from end to start */
    for (size_t i = count; i > 0; i--) {
        result = tcelm_cons(arena, tcelm_char(arena, chars[i-1]), result);
    }

    return result;
}

/*
 * String.fromList : List Char -> String
 */
tcelm_value_t *tcelm_string_fromList(tcelm_arena_t *arena, tcelm_value_t *list) {
    /* First pass: calculate total bytes needed */
    size_t total_bytes = 0;
    size_t char_count = 0;
    tcelm_value_t *curr = list;

    while (!tcelm_is_nil(curr)) {
        tcelm_value_t *head = tcelm_list_head(curr);
        uint32_t cp = TCELM_AS_CHAR(head);
        if (cp < 0x80) total_bytes += 1;
        else if (cp < 0x800) total_bytes += 2;
        else if (cp < 0x10000) total_bytes += 3;
        else total_bytes += 4;
        char_count++;
        curr = tcelm_list_tail(curr);
    }

    /* Allocate result */
    tcelm_value_t *v = tcelm_arena_alloc(arena, sizeof(tcelm_value_t));
    tcelm_string_t *str = tcelm_arena_alloc(arena, sizeof(tcelm_string_t) + total_bytes + 1);

    str->length = total_bytes;
    str->char_count = char_count;

    /* Second pass: encode characters */
    size_t offset = 0;
    curr = list;
    while (!tcelm_is_nil(curr)) {
        tcelm_value_t *head = tcelm_list_head(curr);
        uint32_t cp = TCELM_AS_CHAR(head);
        offset += utf8_encode(cp, str->data + offset);
        curr = tcelm_list_tail(curr);
    }
    str->data[total_bytes] = '\0';

    v->tag = TCELM_TAG_STRING;
    v->data.s = str;
    return v;
}

/*
 * String.toUpper : String -> String
 */
tcelm_value_t *tcelm_string_toUpper(tcelm_arena_t *arena, tcelm_value_t *s) {
    tcelm_string_t *str = TCELM_AS_STRING(s);

    tcelm_value_t *v = tcelm_arena_alloc(arena, sizeof(tcelm_value_t));
    tcelm_string_t *result = tcelm_arena_alloc(arena, sizeof(tcelm_string_t) + str->length + 1);

    result->length = str->length;
    result->char_count = str->char_count;

    /* Simple ASCII uppercase (full Unicode would need ICU or similar) */
    for (size_t i = 0; i < str->length; i++) {
        result->data[i] = (char)toupper((unsigned char)str->data[i]);
    }
    result->data[str->length] = '\0';

    v->tag = TCELM_TAG_STRING;
    v->data.s = result;
    return v;
}

/*
 * String.toLower : String -> String
 */
tcelm_value_t *tcelm_string_toLower(tcelm_arena_t *arena, tcelm_value_t *s) {
    tcelm_string_t *str = TCELM_AS_STRING(s);

    tcelm_value_t *v = tcelm_arena_alloc(arena, sizeof(tcelm_value_t));
    tcelm_string_t *result = tcelm_arena_alloc(arena, sizeof(tcelm_string_t) + str->length + 1);

    result->length = str->length;
    result->char_count = str->char_count;

    for (size_t i = 0; i < str->length; i++) {
        result->data[i] = (char)tolower((unsigned char)str->data[i]);
    }
    result->data[str->length] = '\0';

    v->tag = TCELM_TAG_STRING;
    v->data.s = result;
    return v;
}

/*
 * String.uncons : String -> Maybe (Char, String)
 */
tcelm_value_t *tcelm_string_uncons(tcelm_arena_t *arena, tcelm_value_t *s) {
    tcelm_string_t *str = TCELM_AS_STRING(s);

    if (str->length == 0) {
        /* Nothing */
        return tcelm_custom(arena, 0, "Nothing", 0);
    }

    /* Decode first character */
    const char *ptr = str->data;
    uint32_t first_char = utf8_decode(&ptr);
    size_t first_char_bytes = (size_t)(ptr - str->data);

    /* Create remaining string */
    tcelm_value_t *rest = tcelm_string_len(arena, str->data + first_char_bytes,
                                            str->length - first_char_bytes);

    /* Create tuple (Char, String) */
    tcelm_value_t *tuple = tcelm_tuple2(arena, tcelm_char(arena, first_char), rest);

    /* Just tuple */
    return tcelm_custom(arena, 1, "Just", 1, tuple);
}

/*
 * String.cons : Char -> String -> String
 */
tcelm_value_t *tcelm_string_cons(tcelm_arena_t *arena, tcelm_value_t *c, tcelm_value_t *s) {
    tcelm_string_t *str = TCELM_AS_STRING(s);
    uint32_t cp = TCELM_AS_CHAR(c);

    char char_buf[4];
    size_t char_bytes = utf8_encode(cp, char_buf);

    size_t total_len = char_bytes + str->length;

    tcelm_value_t *v = tcelm_arena_alloc(arena, sizeof(tcelm_value_t));
    tcelm_string_t *result = tcelm_arena_alloc(arena, sizeof(tcelm_string_t) + total_len + 1);

    result->length = total_len;
    result->char_count = str->char_count + 1;
    memcpy(result->data, char_buf, char_bytes);
    memcpy(result->data + char_bytes, str->data, str->length);
    result->data[total_len] = '\0';

    v->tag = TCELM_TAG_STRING;
    v->data.s = result;
    return v;
}

/*
 * String.repeat : Int -> String -> String
 */
tcelm_value_t *tcelm_string_repeat(tcelm_arena_t *arena, tcelm_value_t *n, tcelm_value_t *s) {
    tcelm_string_t *str = TCELM_AS_STRING(s);
    int64_t count = TCELM_AS_INT(n);

    if (count <= 0 || str->length == 0) {
        return tcelm_string(arena, "");
    }

    size_t total_len = str->length * (size_t)count;
    size_t total_chars = str->char_count * (size_t)count;

    tcelm_value_t *v = tcelm_arena_alloc(arena, sizeof(tcelm_value_t));
    tcelm_string_t *result = tcelm_arena_alloc(arena, sizeof(tcelm_string_t) + total_len + 1);

    result->length = total_len;
    result->char_count = total_chars;

    for (int64_t i = 0; i < count; i++) {
        memcpy(result->data + i * str->length, str->data, str->length);
    }
    result->data[total_len] = '\0';

    v->tag = TCELM_TAG_STRING;
    v->data.s = result;
    return v;
}

/*
 * String.reverse : String -> String
 */
tcelm_value_t *tcelm_string_reverse(tcelm_arena_t *arena, tcelm_value_t *s) {
    /* Convert to list, reverse, convert back */
    tcelm_value_t *list = tcelm_string_toList(arena, s);

    /* Reverse the list */
    tcelm_value_t *reversed = TCELM_NIL;
    while (!tcelm_is_nil(list)) {
        reversed = tcelm_cons(arena, tcelm_list_head(list), reversed);
        list = tcelm_list_tail(list);
    }

    return tcelm_string_fromList(arena, reversed);
}

/*
 * String.trim : String -> String
 */
tcelm_value_t *tcelm_string_trim(tcelm_arena_t *arena, tcelm_value_t *s) {
    tcelm_string_t *str = TCELM_AS_STRING(s);

    /* Find start (skip whitespace) */
    size_t start = 0;
    while (start < str->length && isspace((unsigned char)str->data[start])) {
        start++;
    }

    /* Find end (skip whitespace) */
    size_t end = str->length;
    while (end > start && isspace((unsigned char)str->data[end - 1])) {
        end--;
    }

    return tcelm_string_len(arena, str->data + start, end - start);
}

/*
 * String.split : String -> String -> List String
 */
tcelm_value_t *tcelm_string_split(tcelm_arena_t *arena, tcelm_value_t *sep, tcelm_value_t *s) {
    tcelm_string_t *sep_s = TCELM_AS_STRING(sep);
    tcelm_string_t *str = TCELM_AS_STRING(s);

    if (sep_s->length == 0) {
        /* Split into individual characters */
        return tcelm_string_toList(arena, s);
    }

    /* Collect all parts in reverse order, then reverse at the end */
    tcelm_value_t *result = TCELM_NIL;
    const char *start = str->data;
    const char *end = str->data + str->length;
    const char *found;

    while ((found = strstr(start, sep_s->data)) != NULL) {
        /* Add the part before the separator */
        tcelm_value_t *part = tcelm_string_len(arena, start, (size_t)(found - start));
        result = tcelm_cons(arena, part, result);
        start = found + sep_s->length;
    }

    /* Add the remaining part */
    tcelm_value_t *last_part = tcelm_string_len(arena, start, (size_t)(end - start));
    result = tcelm_cons(arena, last_part, result);

    /* Reverse the list */
    tcelm_value_t *reversed = TCELM_NIL;
    while (!tcelm_is_nil(result)) {
        reversed = tcelm_cons(arena, tcelm_list_head(result), reversed);
        result = tcelm_list_tail(result);
    }

    return reversed;
}

/*
 * String.replace : String -> String -> String -> String
 * replace from to string - replaces all occurrences of 'from' with 'to' in 'string'
 */
tcelm_value_t *tcelm_string_replace(tcelm_arena_t *arena, tcelm_value_t *from, tcelm_value_t *to, tcelm_value_t *s) {
    tcelm_string_t *from_s = TCELM_AS_STRING(from);
    tcelm_string_t *to_s = TCELM_AS_STRING(to);
    tcelm_string_t *str = TCELM_AS_STRING(s);

    if (from_s->length == 0) {
        /* Nothing to replace */
        return s;
    }

    /* Count occurrences to calculate result size */
    size_t count = 0;
    const char *p = str->data;
    while ((p = strstr(p, from_s->data)) != NULL) {
        count++;
        p += from_s->length;
    }

    if (count == 0) {
        /* No replacements needed */
        return s;
    }

    /* Calculate new size */
    size_t new_len = str->length + count * (to_s->length - from_s->length);

    tcelm_value_t *v = tcelm_arena_alloc(arena, sizeof(tcelm_value_t));
    tcelm_string_t *result = tcelm_arena_alloc(arena, sizeof(tcelm_string_t) + new_len + 1);

    /* Build result string */
    const char *src = str->data;
    char *dst = result->data;
    const char *found;

    while ((found = strstr(src, from_s->data)) != NULL) {
        /* Copy part before match */
        size_t before_len = (size_t)(found - src);
        memcpy(dst, src, before_len);
        dst += before_len;

        /* Copy replacement */
        memcpy(dst, to_s->data, to_s->length);
        dst += to_s->length;

        src = found + from_s->length;
    }

    /* Copy remaining part */
    size_t remaining = str->length - (size_t)(src - str->data);
    memcpy(dst, src, remaining);
    dst += remaining;
    *dst = '\0';

    result->length = new_len;
    /* Approximate char_count - this is correct for ASCII */
    result->char_count = str->char_count + count * (to_s->char_count - from_s->char_count);

    v->tag = TCELM_TAG_STRING;
    v->data.s = result;
    return v;
}

/*
 * Closure-compatible _impl wrappers
 */

tcelm_value_t *tcelm_string_join_impl(tcelm_arena_t *arena, tcelm_value_t **args) {
    return tcelm_string_join(arena, args[0], args[1]);
}

tcelm_value_t *tcelm_string_append_impl(tcelm_arena_t *arena, tcelm_value_t **args) {
    return tcelm_string_append(arena, args[0], args[1]);
}

tcelm_value_t *tcelm_string_contains_impl(tcelm_arena_t *arena, tcelm_value_t **args) {
    return tcelm_string_contains(arena, args[0], args[1]);
}

tcelm_value_t *tcelm_string_startsWith_impl(tcelm_arena_t *arena, tcelm_value_t **args) {
    return tcelm_string_startsWith(arena, args[0], args[1]);
}

tcelm_value_t *tcelm_string_endsWith_impl(tcelm_arena_t *arena, tcelm_value_t **args) {
    return tcelm_string_endsWith(arena, args[0], args[1]);
}

tcelm_value_t *tcelm_string_isEmpty_impl(tcelm_arena_t *arena, tcelm_value_t **args) {
    return tcelm_string_isEmpty(arena, args[0]);
}

tcelm_value_t *tcelm_string_concat_impl(tcelm_arena_t *arena, tcelm_value_t **args) {
    return tcelm_string_concat(arena, args[0]);
}

tcelm_value_t *tcelm_string_replace_impl(tcelm_arena_t *arena, tcelm_value_t **args) {
    return tcelm_string_replace(arena, args[0], args[1], args[2]);
}

tcelm_value_t *tcelm_string_left_impl(tcelm_arena_t *arena, tcelm_value_t **args) {
    return tcelm_string_left(arena, args[0], args[1]);
}

tcelm_value_t *tcelm_string_right_impl(tcelm_arena_t *arena, tcelm_value_t **args) {
    return tcelm_string_right(arena, args[0], args[1]);
}

tcelm_value_t *tcelm_string_dropLeft_impl(tcelm_arena_t *arena, tcelm_value_t **args) {
    return tcelm_string_dropLeft(arena, args[0], args[1]);
}

tcelm_value_t *tcelm_string_dropRight_impl(tcelm_arena_t *arena, tcelm_value_t **args) {
    return tcelm_string_dropRight(arena, args[0], args[1]);
}

tcelm_value_t *tcelm_string_slice_impl(tcelm_arena_t *arena, tcelm_value_t **args) {
    return tcelm_string_slice(arena, args[0], args[1], args[2]);
}

tcelm_value_t *tcelm_string_length_impl(tcelm_arena_t *arena, tcelm_value_t **args) {
    return tcelm_string_length(arena, args[0]);
}

tcelm_value_t *tcelm_string_repeat_impl(tcelm_arena_t *arena, tcelm_value_t **args) {
    return tcelm_string_repeat(arena, args[0], args[1]);
}

tcelm_value_t *tcelm_string_split_impl(tcelm_arena_t *arena, tcelm_value_t **args) {
    return tcelm_string_split(arena, args[0], args[1]);
}

tcelm_value_t *tcelm_string_toList_impl(tcelm_arena_t *arena, tcelm_value_t **args) {
    return tcelm_string_toList(arena, args[0]);
}

tcelm_value_t *tcelm_string_fromList_impl(tcelm_arena_t *arena, tcelm_value_t **args) {
    return tcelm_string_fromList(arena, args[0]);
}

tcelm_value_t *tcelm_string_toUpper_impl(tcelm_arena_t *arena, tcelm_value_t **args) {
    return tcelm_string_toUpper(arena, args[0]);
}

tcelm_value_t *tcelm_string_toLower_impl(tcelm_arena_t *arena, tcelm_value_t **args) {
    return tcelm_string_toLower(arena, args[0]);
}

tcelm_value_t *tcelm_string_trim_impl(tcelm_arena_t *arena, tcelm_value_t **args) {
    return tcelm_string_trim(arena, args[0]);
}

tcelm_value_t *tcelm_string_reverse_impl(tcelm_arena_t *arena, tcelm_value_t **args) {
    return tcelm_string_reverse(arena, args[0]);
}

tcelm_value_t *tcelm_string_uncons_impl(tcelm_arena_t *arena, tcelm_value_t **args) {
    return tcelm_string_uncons(arena, args[0]);
}

tcelm_value_t *tcelm_string_cons_impl(tcelm_arena_t *arena, tcelm_value_t **args) {
    return tcelm_string_cons(arena, args[0], args[1]);
}

tcelm_value_t *tcelm_string_fromInt_impl(tcelm_arena_t *arena, tcelm_value_t **args) {
    return tcelm_string_fromInt(arena, args[0]);
}

tcelm_value_t *tcelm_string_fromFloat_impl(tcelm_arena_t *arena, tcelm_value_t **args) {
    return tcelm_string_fromFloat(arena, args[0]);
}

tcelm_value_t *tcelm_string_fromChar_impl(tcelm_arena_t *arena, tcelm_value_t **args) {
    return tcelm_string_fromChar(arena, args[0]);
}
