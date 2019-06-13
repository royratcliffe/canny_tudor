:- module(canny_tudor, []).

:- load_files([   docker/random_names,
                  os/search_paths,
                  os/file_searches,
                  os/apps
              ], [if(not_loaded)]).
