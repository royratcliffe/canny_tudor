:- module(canny, []).

:- load_files([   docker/random_names,
                  operating_system/search_paths,
                  operating_system/file_searches
              ], [if(not_loaded)]).
