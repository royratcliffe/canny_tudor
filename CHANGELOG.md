# Change Log

Uses [Semantic Versioning](https://semver.org/). Always [keep a change
log](https://keepachangelog.com/en/1.0.0/).

## [0.24.3] - 2026-01-18
### Added
- Introduced new `roman_numerals//1` grammar for converting integers to Roman
  numeral representations, and back again.
### Fixed
- Minor fix for Docker restyling. Make it optional to restyle keys, falling back
  to original keys if restyling fails.

## [0.24.2] - 2025-12-23
### Added
- Add `ollama_tools` setting to configure tools for Ollama chat interactions.
- Merged `canny_url` module with documentation and tests for url_encoded//1
  and url_coded/2 predicates.

## [0.24.1] - 2025-12-20
### Added
- Introduce a new module for computing prefix sums and range sums, enhancing
  efficiency for range-sum queries. Update documentation and version to reflect
  these changes.

## [0.23.14] - 2025-10-01
### Added
- New predicate `xdigit_weights_and_bytes(?Weights:list(integer), ?Bytes:list(integer))`
  from `canny_bits` module. It relates a list of hexadecimal digit weights
  to a list of corresponding byte values. Each weight is a power of 16, starting
  from the least significant digit (rightmost) with a weight of 1 (16^0), and
  increasing by powers of 16 for each subsequent digit to the left.
- New grammar predicate `xbytes(?Bytes:list(integer))//` from `canny_bits` module.
  It parses a sequence of hexadecimal byte values represented as pairs of
  hexadecimal digit characters (0-9, A-F, a-f) into a list of integers,
  each ranging from 0 to 255. The input is expected to be a sequence of
  hexadecimal digits without any separators, and the total number of digits
  must be even to form complete bytes.

## [0.23.13] - 2025-07-29
### Changed
- Refactor Docker ask predicate for JSON dictionary value restyling
- Enhance ask predicate for list queries and dictionary posts; fix query_search/3
- Improve ask predicate for list queries and JSON request bodies
- Streamline ask predicate for JSON body handling and dictionary processing

## [0.23.12] - 2025-07-27
### Changed
- Remove meta-predicate

## [0.23.11] - 2025-07-27
### Changed
- Enhance Docker predicates and improve documentation

## [0.23.10] - 2025-07-25
### Added
- Docker API
- Placeholder formatting

## [0.23.9] - 2025-06-08
### Added
- Predicate `scasp_just_dot_print(+Stream, +Src, +Options)`

## [0.23.8] - 2025-05-31
### Added
- Predicate `ollama_chat(+Messages:list(dict), -Message:dict, +Options:list)`

## [0.23.7] - 2025-05-11
### Added
- Predicate `directory_entry(+Directory, ?Entry)//`, module `dcg_files`

## [0.23.6] - 2023-09-16
### Added
- Predicate `bit_shift//3`, module `canny_shifter`

## [0.23.5] - 2023-08-29
### Added
- Predicate `endian//3`, module `dcg_endian`

## [0.23.4] - 2023-08-28
### Added
- Predicates `crc`/{2, 3} and `crc_property/2`
- Predicates `crc_16_mcrf4xx`/{2, 3}
- Predicate `rbit/3`
- Table of contents to manual

## [0.23.3] - 2022-10-04
### Added
- Option `key/1` and `id/1` for `xread_call`/{5, 6}
### Fixed
- Cut choice for `threshold/1` option

## [0.23.2] - 2022-10-02
### Changed
- Bumped pack version to patch level 2
- Skipping release for patch level 1

## [0.23.0] - 2022-10-02
### Added
- Half-duplex hdx/{4, 3, 2} predicates

## [0.22.0] - 2022-10-02
### Added
- Canny predicates for Redis and Redis streams

## [0.21.1] - 2022-08-15
### Changed
- Refactored `bit_fields/3` and `bit_fields/4`

## [0.21.0] - 2022-08-13
### Added
- `canny_octet` module

## [0.20.0] - 2022-08-12
### Added
- `canny_z` module

## [0.19.1] - 2022-08-12
### Fixed
- Link to PDF on main branch after switch to simpler GitHub Flow
- Also switch to [Bookman-oriented `tgbonum` font pack](https://www.overleaf.com/learn/latex/Font_typefaces)

## [0.19.0] - 2022-08-03
### Added
- `swi_memfilesio` module

## [0.18.0] - 2021-08-04
### Added
- `swi_settings` module
- `swi_zip` module

## [0.17.0] - 2021-06-15
### Added
- `paxos_udp_broadcast` module for Paxos over UDP broadcast

## [0.16.0] - 2021-06-07
### Added
- `canny_a` module for a_star/3

## [0.15.0] - 2021-06-06
### Added
- `canny_exe` module

## [0.14.0] - 2021-05-05
### Added
- Continuous Integration using GitHub Actions
- Test coverage shields
- Prototype for GitHub API
### Fixed
- Patches for test coverage

## [0.13.0] - 2021-04-12
### Added
- loadavg//5 and loadavg/5 predicates
- current_arch/1, current_arch_os/2 and current_os/1 predicates

## [0.12.0] - 2021-03-13
### Added
- pop_lsbs/2 predicate

## [0.11.0] - 2021-02-18
### Added
- columns_to_rows/2 predicate

## [0.10.0] - 2021-02-15
### Added
- select_options/4 predicate
- comb2/2 predicate

## [0.9.0] - 2020-12-30
### Added
- pairs/2 predicate
### Changed
- indexed_pairs/2 renamed to indexed/2
### Fixed
- pengine_collect/4 filters using pengine_property(Id, self(Id))

## [0.8.3] - 2020-10-17
### Added
- `canny_files` module
- Refactored latex_for_pack/3
- pengine_collect/2, pengine_collect/4 and pengine_wait/1 (swi_pengines module)
- `os_file_searches` refactored to `os_windows`
- prefix_atom_suffix/4

## [0.8.2] - 2020-09-09
### Added
- `canny_arity` module
- payload/1, apply_to/1 and property_of/1 for canny_payloads

## [0.8.1] - 2020-09-04
### Fixed
- Maths predicate remainder/3 to frem/3; avoids clash with remainder//1
- LaTeX manual; omits undocumented modules

## [0.8.0] - 2020-08-29
### Added
- `canny_payloads` module
- `canny_endian` module
- `canny_bits` module
- `ieee_754` module
- LaTeX generator for PDF manual
### Fixed
- Situations now permit non-variable Now terms. This allows for dictionaries
  with unbound tags.
- Situations also now support time-differential calculations with for(Seconds)
  in place of When. Current and previous situations compute the time delay
  between historic situation samples: the difference in time between the current
  and now, or the time delay between the previous and the current.

## [0.7.2] - 2020-07-25
### Added
- append_path/3
- dict_pair/2
- take_at_most/3
- select1/3, select_apply1/3

## [0.7.1] - 2020-06-14
### Added
- `indexed_pairs/{2,3}`
- list_dict/3
- dict_leaf/2
- split_lines/2
### Fixed
- make/0 warnings
- Situation debugging reports WAS, NOW
- Clean up test side effects

## [0.7.0] - 2020-04-10
### Fixed
- Key restyling for dict_compound/2

## [0.6.1] - 2020-04-09
### Added
- restyle_identifier_ex/3
- is_key/1
- dict_compound/2

## [0.6.0] - 2020-04-06
### Added
- permute_sum_of_int/2
- permute_list_to_grid/2
- dict_tag/2
- print_situation_history_lengths/0
- create_dict/3
### Fixed
- Code stylings

## [0.5.2] - 2020-01-11
### Added
- close_streams/2
### Fixed
- Do not independently broadcast was/2 and now/2 for situation transitions

## [0.5.1] - 2019-12-03
### Fixed
- Situation mutator renamed `situation_apply/2`

## [0.5.0] - 2019-12-03
### Added
- Linear algebra
- Canny maths
### Fixed
- Canny situations

## [0.4.0] - 2019-10-19
### Added
- Canny situations
- `random_temporary_module/1` predicate
- `zip/3` predicate (swi_lists)
- `print_table/1` predicate
### Fixed
- `with_output_to/3` uses `random_name_chk/1`

## [0.3.0] - 2019-09-06
### Added
- `random_name_chk/1` versus non-deterministic `random_name/1`

## [0.2.1] - 2019-09-03
### Fixed
- Fix the fix; `dict_member/2` unifies with empty dictionary leaf nodes

## [0.2.0] - 2019-09-02
### Fixed
- Allow dictionary leaf values for `dict_member/2`

## [0.1.1] - 2019-08-02
### Added
- Missing pack maintainer, home and download links

## [0.1.0] - 2019-08-02
### Added
- Initial spike
