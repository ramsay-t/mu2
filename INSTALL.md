Installing
===

1) Make sure the Wrangler sources are installed (see: https://github.com/RefactoringTools/Wrangler).

2) Set the correct path to the Wrangler sources in both Makefile and include/install.hrl.

3) Type make

The shell scripts in bin/ provide starting points for common usages. The mu2 script will generate a specified number of mutants into a specified folder, applying one mutation randomly chosen from all the applicable mutations.

The apply_logging script will insert logging calls into all functions to track the variable values used in calls. When the mutated module is called it will generate a file named &lt;module&gt;.log that contains one line per function call, in the format: &lt;function name&gt; [arguments list]
