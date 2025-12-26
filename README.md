# SystemLisp

SystemLisp is an experimental HDL (Hardware Description Language) simulator written in Common Lisp focused on interactive and extensible hardware design and verification.

## Why Common Lisp?

- **REPL-based programming** - Test ideas and see results instantly without a full compile–run cycle. RTL and testbenches can be incrementally built and modified while the simulation is running, reducing context-switching and enabling faster debugging.
- **Extensibility** - Common Lisp macros can be used to extend the language with new domain-specific constructs, including HDL-style syntax and programming idioms borrowed from existing languages.
- **Homoiconicity** - Lisp's symbolic expressions naturally represent abstract syntax trees (ASTs). This makes it straightforward to analyze, transform, and translate code from existing HDLs such as SystemVerilog, VHDL, or Specman-e into Common Lisp.
- **Powerful Object System** - The Common Lisp Object System (CLOS) is one of the oldest and most powerful implementations of the object-oriented programming paradigm which provides features such as:
  - **Multiple dispatch**: Methods can specialize on the types (and values) of multiple arguments, leading to very expressive and modular designs, and helping solve the “expression problem”common in simulators and compilers.
  - **Runtime flexibility**: Classes, methods, and even inheritance relationships can be modified while the simulation is running, enabling highly interactive and exploratory modeling.
  - **Metaobject Protocol (MOP)**: Provides hooks to customize the object system itself for advanced use cases.

## Quick Start

### Prerequisites

- **Common Lisp Implementation**: SBCL (recommended) or ECL
- **Quicklisp**: This is the package manager for Common Lisp, see installation instructions [here](https://www.quicklisp.org/beta/#installation)
- **Emacs+Sly/SLIME**: To have a Common Lisp IDE make sure you have Emacs with the Sly (recommended) or the SLIME plugins installed
  - **Portacle (optional)**: Alternatively, if you want to avoid doing all the Emacs setup yourself you can install the [Portacle](https://portacle.github.io/) bundle which includes Emacs, SBCL, SLIME and a few other useful plugins for Common Lisp development
- **SQLite3**: SystemLisp stores waveform data in SQLite database files
- **rlwrap (optional)**: A command-line utility that adds powerful GNU Readline features, like command history, line editing, and programmable tab completion, to any program that reads from standard input but doesn't support readline natively
- **GTKWave (optional)**: A free and open-source waveform viewer

### Installation

Since SystemLisp is not yet in Quicklisp, install manually:

```bash
# SystemLisp must be cloned inside the local-projects folder of the quicklisp installation
$ cd $HOME/quicklisp/local-projects

# Clone the repository
$ git clone https://github.com/systemlisp/systemlisp.git

# Open the Common Lisp REPL either in the shell or inside Emacs using Sly or SLIME
$ sbcl
# or "rlwrap sbcl"

# Install the system-lisp package and its dependencies
* (ql:quickload "system-lisp")
```

### Running the examples

Because SystemLisp simulations dump vcd waveform files and sqlite3 databases with unique names after every run it's recommended to run them from a dedicated folder

```bash
# Create dedicated folder
$ mkdir my-sim-folder
$ cd my-sim-folder

# Start the CL REPL (sbcl in the shell or Sly/SLIME in Emacs)
$ sbcl
# or "rlwrap sbcl"

# Load the SystemLisp
* (ql:quickload "system-lisp")

# Load the SystemLisp examples package
* (ql:quickload "system-lisp/examples")

# Enter the :sl-examples package
* (in-package :sl-examples)

# Run one of the following examples
* (counter-run-sim)
* (d-flip-flop-run-sim)
* (rand-num-gen-run-sim)
* (run-spi-test-1master-1slave-sim)
* (swap-tb-run-sim)

# Exit sbcl
* (uiop:quit)

# Open the waveform file
$ gtkwave waves.vcd
```

Notice that after running an example a waves.vcd file is dumped which can be opened in GtkWave or some other waveform viewer to view signal waveforms. There are also some sqlite3 database files generated. These also contain waveform data in a SystemLisp-specific format. SystemLisp generates the vcd waveform files from these sqlite3 database files. Once the vcd file has been generated you can delete the sqlite3 database since it's not needed anymore.

More examples will be added in the future.

### Running the unit tests

The unit tests should also be run in a dedicated folder because each test dumps sqlite3 database files which will have to be deleted manually afterwards.

```bash
# Create dedicated folder
$ mkdir my-regr-folder
$ cd my-regr-folder

# Start the CL REPL (sbcl in the shell or Sly/SLIME in Emacs)
sbcl

# Load the SystemLisp
* (ql:quickload "system-lisp")

# Load the SystemLisp test package
* (ql:quickload "system-lisp/tests")

# Enter the :system-lisp-test package
* (in-package :system-lisp-test)

# Run the unit tests
* (run-all-tests)
```

At the moment there are two known failures in the unit test regression which are being debugged.
