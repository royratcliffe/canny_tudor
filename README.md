# Canny bag o' Tudor

See [PDF](https://github.com/royratcliffe/canny_tudor/blob/master/man/canny_tudor.pdf) for details.

This is an experimental SWI-Prolog 'pack' comprising technical spikes, or
otherwise useful, Prolog predicates that do not seem to fit anywhere else.

The package name reflects a mixed bag of bits and pieces. It's a phrase from the
North-East corner of England, United Kingdom. 'Canny' means nice, or good. Tudor
is a crisp (chip, in American) manufacturer. This pack comprises various
unrelated predicates that may, or may not, be tasty; like crisps in a bag, the
library sub-folders and module names delineate the disparate components. If the
sub-modules grow to warrant a larger division, they can ultimately fork their
own pack.

The pack currently includes:

* Docker-style random names
* Operating system-related features:
  * Search path manipulation
  * Start and stop, upping and downing apps
* SWI-Prolog extensions for dictionaries and compounds

The pack comprises experimental modules subject to change and revision
due to its nature. The pack's major version will always remain 0.
Work in progress!

## Apps

You can start or stop an app.

    app_start(App)
    app_stop(App)

App is some compound that identifies which app to start and stop. You define an
App using `os:property_for_app/2` multi-file predicate. You must at least define
an app's path using, as an example:

    os:property_for_app(path(path(mspaint)), mspaint) :- !.

Note that the Path is a path Spec used by `process_create/3`, so can include a
path-relative term as above. This is enough to launch the Microsoft Paint app on
Windows. No need for arguments and options for this example. Starting a _running_
app does not start a new instance. Rather, it succeeds for the existing
instance. The green cut prevents unnecessary backtracking.

You can start and continuously restart apps using `app_up/1`, and subsequently
shut them down with `app_down/1`.

### Apps testing

On a Windows system, try the following for example. It launches Microsoft Paint.
Exit the Paint app after `app_up/1` below and it will relaunch automatically.

```prolog
?- [library(os/apps), library(os/apps_testing)].
true.

?- app_up(mspaint).
true.

?- app_down(mspaint).
true.
```

## SWI-Prolog extensions

This includes the following.

### Non-deterministic `dict_member(?Dict, ?Member)`

This predicate offers an alternative approach to dictionary iteration in
Prolog. It makes a dictionary expose its leaves as a list exposes its
elements, one by one non-deterministically. It does not unify with
non-leaves, as for empty dictionaries.

```prolog
?- dict_member(a{b:c{d:e{f:g{h:i{j:999}}}}}, Key-Value).
Key = a^b/c^d/e^f/g^h/i^j,
Value = 999.

?- dict_member(Dict, a^b/c^d/e^f/g^h/i^j-999).
Dict = a{b:c{d:e{f:g{h:i{j:999}}}}}.
```
