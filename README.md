# sample-profiler

Runs an external probabilistic sampling profiler over a lisp image, interprets
its output, and produces an HTML file with the profiling results in the
form of a collapsible call tree.  Each node in the tree shows the number of samples
followed by the description of the function sampled.

Currently only works in CCL on MacOS X (using the `sample` program).

## Usage

The following symbols are defined in the `sample-profiler` package:

`(start-profiling &key (seconds T) (verbose nil) (output-file "profile.html"))`

Starts profiling the current lisp image.  `seconds` can be a number to automatically stop profiling after that many seconds.  The default is `T` meaning don't stop until requested.  `output-file` specifies the default file to use for output.  The file is not actually created until `end-profiling` is called, and can be overridden at that time.  If `verbose` is true, shows output from the external profiler process.

`(end-profiling &key abort (verbose nil) output-file)`

Ends the current profiling session.  If `abort` is specified, kills the profiler process without waiting for the result, otherwise tries to terminate gracefully and process the results into the output file.  if `output-file` is specified, it overrides the value given in the call to `start-profiling`.  If `verbose` is true, shows output from the external profiler process.


`*show-foreign-address*`

If true, descriptions of foreign functions include their hex address.  The default is `NIL`.


## Known bugs and missing features

* Only recognizes functions created before `start-profiling` is called.  Functions created during the profiling session (by invoking the compiler or loading fasl files) are not recognized and show up as hex addresses in the dynamic heap.

* Doesn't work on linux. (Need something similar to MacOsX's `sample` program.)

* In addition to the count, should show percentage of total samples.

* Should be able to filter on percentage, not just count

* Should be able to specify expand to max depth

* Should combine sibling entries of the same lisp function into one entry.

* Need an option to expand-all-children for any node.

* Need an option to set root to any node (i.e. focus on a branch).

* Option to combine samples at different offsets of the same function.

* There should be an option to automatically open a browser window with the output.  In the IDE, might want to open a webview within the CCL app.
