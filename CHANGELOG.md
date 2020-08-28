# Change Log

Uses [Semantic Versioning](https://semver.org/). Always [keep a change
log](https://keepachangelog.com/en/1.0.0/).

## [0.7.2] - 2020-07-25
### Added
- append_path/3
- dict_pair/2
- take_at_most/3
- select1/3, select_apply1/3

## [0.7.1] - 2020-06-14
### Added
- indexed_pairs/{2,3}
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
