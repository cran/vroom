// Generated by cpp11: do not edit by hand
// clang-format off

#include "vroom_types.h"
#include "cpp11/declarations.hpp"
#include <R_ext/Visibility.h>

// altrep.cc
void force_materialization(SEXP x);
extern "C" SEXP _vroom_force_materialization(SEXP x) {
  BEGIN_CPP11
    force_materialization(cpp11::as_cpp<cpp11::decay_t<SEXP>>(x));
    return R_NilValue;
  END_CPP11
}
// altrep.cc
SEXP vroom_materialize(SEXP x, bool replace);
extern "C" SEXP _vroom_vroom_materialize(SEXP x, SEXP replace) {
  BEGIN_CPP11
    return cpp11::as_sexp(vroom_materialize(cpp11::as_cpp<cpp11::decay_t<SEXP>>(x), cpp11::as_cpp<cpp11::decay_t<bool>>(replace)));
  END_CPP11
}
// altrep.cc
SEXP vroom_convert(SEXP x);
extern "C" SEXP _vroom_vroom_convert(SEXP x) {
  BEGIN_CPP11
    return cpp11::as_sexp(vroom_convert(cpp11::as_cpp<cpp11::decay_t<SEXP>>(x)));
  END_CPP11
}
// altrep.cc
std::string vroom_str_(const cpp11::sexp& x);
extern "C" SEXP _vroom_vroom_str_(SEXP x) {
  BEGIN_CPP11
    return cpp11::as_sexp(vroom_str_(cpp11::as_cpp<cpp11::decay_t<const cpp11::sexp&>>(x)));
  END_CPP11
}
// gen.cc
cpp11::strings gen_character_(int n, int min, int max, std::string values, uint32_t seed, uint32_t seed2);
extern "C" SEXP _vroom_gen_character_(SEXP n, SEXP min, SEXP max, SEXP values, SEXP seed, SEXP seed2) {
  BEGIN_CPP11
    return cpp11::as_sexp(gen_character_(cpp11::as_cpp<cpp11::decay_t<int>>(n), cpp11::as_cpp<cpp11::decay_t<int>>(min), cpp11::as_cpp<cpp11::decay_t<int>>(max), cpp11::as_cpp<cpp11::decay_t<std::string>>(values), cpp11::as_cpp<cpp11::decay_t<uint32_t>>(seed), cpp11::as_cpp<cpp11::decay_t<uint32_t>>(seed2)));
  END_CPP11
}
// guess_type.cc
std::string guess_type_(cpp11::writable::strings input, const cpp11::strings& na, const cpp11::list& locale, bool guess_integer);
extern "C" SEXP _vroom_guess_type_(SEXP input, SEXP na, SEXP locale, SEXP guess_integer) {
  BEGIN_CPP11
    return cpp11::as_sexp(guess_type_(cpp11::as_cpp<cpp11::decay_t<cpp11::writable::strings>>(input), cpp11::as_cpp<cpp11::decay_t<const cpp11::strings&>>(na), cpp11::as_cpp<cpp11::decay_t<const cpp11::list&>>(locale), cpp11::as_cpp<cpp11::decay_t<bool>>(guess_integer)));
  END_CPP11
}
// iconv_file.cc
size_t convert_connection(SEXP in_con, SEXP out_con, const std::string& from, const std::string& to);
extern "C" SEXP _vroom_convert_connection(SEXP in_con, SEXP out_con, SEXP from, SEXP to) {
  BEGIN_CPP11
    return cpp11::as_sexp(convert_connection(cpp11::as_cpp<cpp11::decay_t<SEXP>>(in_con), cpp11::as_cpp<cpp11::decay_t<SEXP>>(out_con), cpp11::as_cpp<cpp11::decay_t<const std::string&>>(from), cpp11::as_cpp<cpp11::decay_t<const std::string&>>(to)));
  END_CPP11
}
// vroom_dttm.cc
cpp11::writable::doubles utctime_(const cpp11::integers& year, const cpp11::integers& month, const cpp11::integers& day, const cpp11::integers& hour, const cpp11::integers& min, const cpp11::integers& sec, const cpp11::doubles& psec);
extern "C" SEXP _vroom_utctime_(SEXP year, SEXP month, SEXP day, SEXP hour, SEXP min, SEXP sec, SEXP psec) {
  BEGIN_CPP11
    return cpp11::as_sexp(utctime_(cpp11::as_cpp<cpp11::decay_t<const cpp11::integers&>>(year), cpp11::as_cpp<cpp11::decay_t<const cpp11::integers&>>(month), cpp11::as_cpp<cpp11::decay_t<const cpp11::integers&>>(day), cpp11::as_cpp<cpp11::decay_t<const cpp11::integers&>>(hour), cpp11::as_cpp<cpp11::decay_t<const cpp11::integers&>>(min), cpp11::as_cpp<cpp11::decay_t<const cpp11::integers&>>(sec), cpp11::as_cpp<cpp11::decay_t<const cpp11::doubles&>>(psec)));
  END_CPP11
}
// vroom_errors.cpp
cpp11::data_frame vroom_errors_(cpp11::external_pointer<std::shared_ptr<vroom_errors>> errors);
extern "C" SEXP _vroom_vroom_errors_(SEXP errors) {
  BEGIN_CPP11
    return cpp11::as_sexp(vroom_errors_(cpp11::as_cpp<cpp11::decay_t<cpp11::external_pointer<std::shared_ptr<vroom_errors>>>>(errors)));
  END_CPP11
}
// vroom_fwf.cc
cpp11::list vroom_fwf_(const cpp11::list& inputs, const std::vector<int>& col_starts, const std::vector<int>& col_ends, bool trim_ws, cpp11::sexp col_names, cpp11::sexp col_types, cpp11::sexp col_select, cpp11::sexp name_repair, size_t skip, const char* comment, bool skip_empty_rows, ptrdiff_t n_max, SEXP id, const cpp11::strings& na, const cpp11::list& locale, ptrdiff_t guess_max, size_t num_threads, size_t altrep, bool progress);
extern "C" SEXP _vroom_vroom_fwf_(SEXP inputs, SEXP col_starts, SEXP col_ends, SEXP trim_ws, SEXP col_names, SEXP col_types, SEXP col_select, SEXP name_repair, SEXP skip, SEXP comment, SEXP skip_empty_rows, SEXP n_max, SEXP id, SEXP na, SEXP locale, SEXP guess_max, SEXP num_threads, SEXP altrep, SEXP progress) {
  BEGIN_CPP11
    return cpp11::as_sexp(vroom_fwf_(cpp11::as_cpp<cpp11::decay_t<const cpp11::list&>>(inputs), cpp11::as_cpp<cpp11::decay_t<const std::vector<int>&>>(col_starts), cpp11::as_cpp<cpp11::decay_t<const std::vector<int>&>>(col_ends), cpp11::as_cpp<cpp11::decay_t<bool>>(trim_ws), cpp11::as_cpp<cpp11::decay_t<cpp11::sexp>>(col_names), cpp11::as_cpp<cpp11::decay_t<cpp11::sexp>>(col_types), cpp11::as_cpp<cpp11::decay_t<cpp11::sexp>>(col_select), cpp11::as_cpp<cpp11::decay_t<cpp11::sexp>>(name_repair), cpp11::as_cpp<cpp11::decay_t<size_t>>(skip), cpp11::as_cpp<cpp11::decay_t<const char*>>(comment), cpp11::as_cpp<cpp11::decay_t<bool>>(skip_empty_rows), cpp11::as_cpp<cpp11::decay_t<ptrdiff_t>>(n_max), cpp11::as_cpp<cpp11::decay_t<SEXP>>(id), cpp11::as_cpp<cpp11::decay_t<const cpp11::strings&>>(na), cpp11::as_cpp<cpp11::decay_t<const cpp11::list&>>(locale), cpp11::as_cpp<cpp11::decay_t<ptrdiff_t>>(guess_max), cpp11::as_cpp<cpp11::decay_t<size_t>>(num_threads), cpp11::as_cpp<cpp11::decay_t<size_t>>(altrep), cpp11::as_cpp<cpp11::decay_t<bool>>(progress)));
  END_CPP11
}
// vroom_fwf.cc
cpp11::list whitespace_columns_(const std::string& filename, size_t skip, ptrdiff_t n, const std::string& comment);
extern "C" SEXP _vroom_whitespace_columns_(SEXP filename, SEXP skip, SEXP n, SEXP comment) {
  BEGIN_CPP11
    return cpp11::as_sexp(whitespace_columns_(cpp11::as_cpp<cpp11::decay_t<const std::string&>>(filename), cpp11::as_cpp<cpp11::decay_t<size_t>>(skip), cpp11::as_cpp<cpp11::decay_t<ptrdiff_t>>(n), cpp11::as_cpp<cpp11::decay_t<const std::string&>>(comment)));
  END_CPP11
}
// vroom_write.cc
void vroom_write_(const cpp11::list& input, const std::string& filename, const char delim, const std::string& eol, const char* na_str, bool col_names, bool append, size_t options, size_t num_threads, bool progress, size_t buf_lines);
extern "C" SEXP _vroom_vroom_write_(SEXP input, SEXP filename, SEXP delim, SEXP eol, SEXP na_str, SEXP col_names, SEXP append, SEXP options, SEXP num_threads, SEXP progress, SEXP buf_lines) {
  BEGIN_CPP11
    vroom_write_(cpp11::as_cpp<cpp11::decay_t<const cpp11::list&>>(input), cpp11::as_cpp<cpp11::decay_t<const std::string&>>(filename), cpp11::as_cpp<cpp11::decay_t<const char>>(delim), cpp11::as_cpp<cpp11::decay_t<const std::string&>>(eol), cpp11::as_cpp<cpp11::decay_t<const char*>>(na_str), cpp11::as_cpp<cpp11::decay_t<bool>>(col_names), cpp11::as_cpp<cpp11::decay_t<bool>>(append), cpp11::as_cpp<cpp11::decay_t<size_t>>(options), cpp11::as_cpp<cpp11::decay_t<size_t>>(num_threads), cpp11::as_cpp<cpp11::decay_t<bool>>(progress), cpp11::as_cpp<cpp11::decay_t<size_t>>(buf_lines));
    return R_NilValue;
  END_CPP11
}
// vroom_write.cc
void vroom_write_connection_(const cpp11::list& input, const cpp11::sexp& con, const char delim, const std::string& eol, const char* na_str, bool col_names, size_t options, size_t num_threads, bool progress, size_t buf_lines, bool is_stdout, bool append);
extern "C" SEXP _vroom_vroom_write_connection_(SEXP input, SEXP con, SEXP delim, SEXP eol, SEXP na_str, SEXP col_names, SEXP options, SEXP num_threads, SEXP progress, SEXP buf_lines, SEXP is_stdout, SEXP append) {
  BEGIN_CPP11
    vroom_write_connection_(cpp11::as_cpp<cpp11::decay_t<const cpp11::list&>>(input), cpp11::as_cpp<cpp11::decay_t<const cpp11::sexp&>>(con), cpp11::as_cpp<cpp11::decay_t<const char>>(delim), cpp11::as_cpp<cpp11::decay_t<const std::string&>>(eol), cpp11::as_cpp<cpp11::decay_t<const char*>>(na_str), cpp11::as_cpp<cpp11::decay_t<bool>>(col_names), cpp11::as_cpp<cpp11::decay_t<size_t>>(options), cpp11::as_cpp<cpp11::decay_t<size_t>>(num_threads), cpp11::as_cpp<cpp11::decay_t<bool>>(progress), cpp11::as_cpp<cpp11::decay_t<size_t>>(buf_lines), cpp11::as_cpp<cpp11::decay_t<bool>>(is_stdout), cpp11::as_cpp<cpp11::decay_t<bool>>(append));
    return R_NilValue;
  END_CPP11
}
// vroom_write.cc
cpp11::strings vroom_format_(const cpp11::list& input, const char delim, const std::string& eol, const char* na_str, bool col_names, bool append, size_t options, size_t num_threads, bool progress, size_t buf_lines);
extern "C" SEXP _vroom_vroom_format_(SEXP input, SEXP delim, SEXP eol, SEXP na_str, SEXP col_names, SEXP append, SEXP options, SEXP num_threads, SEXP progress, SEXP buf_lines) {
  BEGIN_CPP11
    return cpp11::as_sexp(vroom_format_(cpp11::as_cpp<cpp11::decay_t<const cpp11::list&>>(input), cpp11::as_cpp<cpp11::decay_t<const char>>(delim), cpp11::as_cpp<cpp11::decay_t<const std::string&>>(eol), cpp11::as_cpp<cpp11::decay_t<const char*>>(na_str), cpp11::as_cpp<cpp11::decay_t<bool>>(col_names), cpp11::as_cpp<cpp11::decay_t<bool>>(append), cpp11::as_cpp<cpp11::decay_t<size_t>>(options), cpp11::as_cpp<cpp11::decay_t<size_t>>(num_threads), cpp11::as_cpp<cpp11::decay_t<bool>>(progress), cpp11::as_cpp<cpp11::decay_t<size_t>>(buf_lines)));
  END_CPP11
}
// vroom.cc
SEXP vroom_(const cpp11::list& inputs, SEXP delim, const char quote, bool trim_ws, bool escape_double, bool escape_backslash, const char* comment, const bool skip_empty_rows, size_t skip, ptrdiff_t n_max, bool progress, const cpp11::sexp& col_names, cpp11::sexp col_types, cpp11::sexp col_select, cpp11::sexp name_repair, SEXP id, const cpp11::strings& na, const cpp11::list& locale, ptrdiff_t guess_max, size_t num_threads, size_t altrep);
extern "C" SEXP _vroom_vroom_(SEXP inputs, SEXP delim, SEXP quote, SEXP trim_ws, SEXP escape_double, SEXP escape_backslash, SEXP comment, SEXP skip_empty_rows, SEXP skip, SEXP n_max, SEXP progress, SEXP col_names, SEXP col_types, SEXP col_select, SEXP name_repair, SEXP id, SEXP na, SEXP locale, SEXP guess_max, SEXP num_threads, SEXP altrep) {
  BEGIN_CPP11
    return cpp11::as_sexp(vroom_(cpp11::as_cpp<cpp11::decay_t<const cpp11::list&>>(inputs), cpp11::as_cpp<cpp11::decay_t<SEXP>>(delim), cpp11::as_cpp<cpp11::decay_t<const char>>(quote), cpp11::as_cpp<cpp11::decay_t<bool>>(trim_ws), cpp11::as_cpp<cpp11::decay_t<bool>>(escape_double), cpp11::as_cpp<cpp11::decay_t<bool>>(escape_backslash), cpp11::as_cpp<cpp11::decay_t<const char*>>(comment), cpp11::as_cpp<cpp11::decay_t<const bool>>(skip_empty_rows), cpp11::as_cpp<cpp11::decay_t<size_t>>(skip), cpp11::as_cpp<cpp11::decay_t<ptrdiff_t>>(n_max), cpp11::as_cpp<cpp11::decay_t<bool>>(progress), cpp11::as_cpp<cpp11::decay_t<const cpp11::sexp&>>(col_names), cpp11::as_cpp<cpp11::decay_t<cpp11::sexp>>(col_types), cpp11::as_cpp<cpp11::decay_t<cpp11::sexp>>(col_select), cpp11::as_cpp<cpp11::decay_t<cpp11::sexp>>(name_repair), cpp11::as_cpp<cpp11::decay_t<SEXP>>(id), cpp11::as_cpp<cpp11::decay_t<const cpp11::strings&>>(na), cpp11::as_cpp<cpp11::decay_t<const cpp11::list&>>(locale), cpp11::as_cpp<cpp11::decay_t<ptrdiff_t>>(guess_max), cpp11::as_cpp<cpp11::decay_t<size_t>>(num_threads), cpp11::as_cpp<cpp11::decay_t<size_t>>(altrep)));
  END_CPP11
}
// vroom.cc
bool has_trailing_newline(const cpp11::strings& filename);
extern "C" SEXP _vroom_has_trailing_newline(SEXP filename) {
  BEGIN_CPP11
    return cpp11::as_sexp(has_trailing_newline(cpp11::as_cpp<cpp11::decay_t<const cpp11::strings&>>(filename)));
  END_CPP11
}
// vroom.cc
SEXP vroom_rle(const cpp11::integers& input);
extern "C" SEXP _vroom_vroom_rle(SEXP input) {
  BEGIN_CPP11
    return cpp11::as_sexp(vroom_rle(cpp11::as_cpp<cpp11::decay_t<const cpp11::integers&>>(input)));
  END_CPP11
}

extern "C" {
static const R_CallMethodDef CallEntries[] = {
    {"_vroom_convert_connection",      (DL_FUNC) &_vroom_convert_connection,       4},
    {"_vroom_force_materialization",   (DL_FUNC) &_vroom_force_materialization,    1},
    {"_vroom_gen_character_",          (DL_FUNC) &_vroom_gen_character_,           6},
    {"_vroom_guess_type_",             (DL_FUNC) &_vroom_guess_type_,              4},
    {"_vroom_has_trailing_newline",    (DL_FUNC) &_vroom_has_trailing_newline,     1},
    {"_vroom_utctime_",                (DL_FUNC) &_vroom_utctime_,                 7},
    {"_vroom_vroom_",                  (DL_FUNC) &_vroom_vroom_,                  21},
    {"_vroom_vroom_convert",           (DL_FUNC) &_vroom_vroom_convert,            1},
    {"_vroom_vroom_errors_",           (DL_FUNC) &_vroom_vroom_errors_,            1},
    {"_vroom_vroom_format_",           (DL_FUNC) &_vroom_vroom_format_,           10},
    {"_vroom_vroom_fwf_",              (DL_FUNC) &_vroom_vroom_fwf_,              19},
    {"_vroom_vroom_materialize",       (DL_FUNC) &_vroom_vroom_materialize,        2},
    {"_vroom_vroom_rle",               (DL_FUNC) &_vroom_vroom_rle,                1},
    {"_vroom_vroom_str_",              (DL_FUNC) &_vroom_vroom_str_,               1},
    {"_vroom_vroom_write_",            (DL_FUNC) &_vroom_vroom_write_,            11},
    {"_vroom_vroom_write_connection_", (DL_FUNC) &_vroom_vroom_write_connection_, 12},
    {"_vroom_whitespace_columns_",     (DL_FUNC) &_vroom_whitespace_columns_,      4},
    {NULL, NULL, 0}
};
}

void init_vroom_big_int(DllInfo* dll);
void init_vroom_chr(DllInfo* dll);
void init_vroom_date(DllInfo* dll);
void init_vroom_dbl(DllInfo* dll);
void init_vroom_dttm(DllInfo* dll);
void init_vroom_fct(DllInfo* dll);
void init_vroom_int(DllInfo* dll);
void init_vroom_num(DllInfo* dll);
void init_vroom_rle(DllInfo* dll);
void init_vroom_time(DllInfo* dll);

extern "C" attribute_visible void R_init_vroom(DllInfo* dll){
  R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
  R_useDynamicSymbols(dll, FALSE);
  init_vroom_big_int(dll);
  init_vroom_chr(dll);
  init_vroom_date(dll);
  init_vroom_dbl(dll);
  init_vroom_dttm(dll);
  init_vroom_fct(dll);
  init_vroom_int(dll);
  init_vroom_num(dll);
  init_vroom_rle(dll);
  init_vroom_time(dll);
  R_forceSymbols(dll, TRUE);
}
