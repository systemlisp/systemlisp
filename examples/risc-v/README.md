# RISC-V Example: FemtoRV32 Processor Simulation

This example demonstrates SystemLisp's capabilities by simulating a RISC-V
RV32I processor. The CPU implementation is inspired by Bruno Levy's
[learn-fpga](https://github.com/BrunoLevy/learn-fpga) project, specifically
the FemtoRV32 minimal processor design.

## Files

| File | Description |
|------|-------------|
| `femtorv32.lisp` | FemtoRV32 CPU implementation (~830 lines) |
| `dram-beh.lisp` | Behavioral DRAM model (~90 lines) |
| `femtorv32-dram-tb.lisp` | Testbench: top-level component wiring CPU + DRAM |
| `rv32i-asm.lisp` | RV32I assembler (~520 lines) |
| `rv32i-asm-test.lisp` | Unit tests for instruction encoding |
| `femtorv32-prog-test.lisp` | 34 program-level tests run on the CPU model |
| `femtorv32-tb.lisp` | Legacy testbench (earlier version) |

## Testbench Architecture

The testbench instantiates a `femtorv32-dram-top` component that wires
together the CPU and DRAM with shared clock and reset signals.

```
                    femtorv32-dram-top
  ┌─────────────────────────────────────────────────┐
  │                                                 │
  │  clk ──────────┬──────────────────┐             │
  │  reset ────────┤                  │             │
  │                ▼                  ▼             │
  │  ┌──────────────────┐   ┌──────────────────┐   │
  │  │    femtorv32      │   │    dram-beh      │   │
  │  │    (CPU)          │   │    (Memory)      │   │
  │  │                   │   │                  │   │
  │  │ mem-addr ────────────▶ mem-addr         │   │
  │  │ mem-wdata ───────────▶ mem-wdata        │   │
  │  │ mem-wmask ───────────▶ mem-wmask        │   │
  │  │ mem-rstrb ───────────▶ mem-rstrb        │   │
  │  │                   │   │                  │   │
  │  │ mem-rdata ◀────────── mem-rdata         │   │
  │  │ mem-rbusy ◀────────── mem-rbusy         │   │
  │  │ mem-wbusy ◀────────── mem-wbusy         │   │
  │  └──────────────────┘   └──────────────────┘   │
  └─────────────────────────────────────────────────┘
```

Signal connections are established in the `connect` method using
`connect-driver-load`, which links a driver signal to a load signal so that
value changes propagate automatically:

```lisp
(defmethod connect ((self femtorv32-dram-top))
  ;; Clock and reset to both children
  (connect-driver-load (-> self clk) (-> self cpu clk))
  (connect-driver-load (-> self clk) (-> self dram clk))
  (connect-driver-load (-> self reset) (-> self cpu reset))
  (connect-driver-load (-> self reset) (-> self dram reset))
  ;; CPU outputs -> DRAM inputs
  (connect-driver-load (-> self cpu mem-addr) (-> self dram mem-addr))
  (connect-driver-load (-> self cpu mem-wdata) (-> self dram mem-wdata))
  (connect-driver-load (-> self cpu mem-wmask) (-> self dram mem-wmask))
  (connect-driver-load (-> self cpu mem-rstrb) (-> self dram mem-rstrb))
  ;; DRAM outputs -> CPU inputs
  (connect-driver-load (-> self dram mem-rdata) (-> self cpu mem-rdata))
  (connect-driver-load (-> self dram mem-rbusy) (-> self cpu mem-rbusy))
  (connect-driver-load (-> self dram mem-wbusy) (-> self cpu mem-wbusy)))
```

### FemtoRV32 CPU

The processor implements a subset of the RV32I base integer instruction set
with a 4-state FSM using one-hot encoding:

| State | Encoding | Description |
|-------|----------|-------------|
| FETCH_INSTR | 1 | Default state, issues memory read for next instruction |
| WAIT_INSTR | 2 | Waits for memory read to complete, latches instruction and register operands |
| EXECUTE | 4 | Executes the instruction, updates PC, issues memory requests for loads/stores |
| WAIT_ALU_OR_MEM | 8 | Waits for shift operations or memory accesses to complete |

Key design details:
- 24-bit program counter (16 MB address space)
- 32-entry register file (x0 hardwired to zero)
- Combinatorial ALU with separate shift unit (multi-cycle for shifts)
- Instruction decoding via combinatorial signals (`is-load`, `is-alu-imm`, `is-branch`, etc.)
- All immediate formats decoded in parallel (Uimm, Iimm, Simm, Bimm, Jimm)
- Byte/halfword/word load and store support with proper sign extension

The CPU uses four spawned processes for its sequential logic:

```lisp
(defmethod run ((self femtorv32))
  (spawn #'regfile-logic self)    ; register file write-back
  (spawn #'shift-logic self)      ; multi-cycle shift operations
  (spawn #'fsm-logic self)        ; main state machine
  (spawn #'cycle-counter-logic self))
```

### Behavioral DRAM Model

The DRAM (`dram-beh`) is a behavioral memory model with a simple
request/acknowledge handshake protocol. It uses a 3-state FSM:

| State | Description |
|-------|-------------|
| WAIT_FOR_REQ | Idle, watching for read strobe (`mem-rstrb`) or write mask (`mem-wmask`) |
| SERVE_READ | Read in progress, data appears on `mem-rdata` next cycle, then deasserts `mem-rbusy` |
| SERVE_WRITE | Write in progress, performs read-modify-write for partial-word writes, then deasserts `mem-wbusy` |

The memory interface is byte-addressed from the CPU's perspective, but the
DRAM stores 32-bit words. Address translation is done by right-shifting the
byte address by 2 (`ash addr -2`).

For partial-word writes (byte or halfword stores), the DRAM performs a
read-modify-write using a byte-enable-to-bit-mask conversion:

```lisp
(let* ((addr (ash (-> self mem-addr value sl-uint-value) -2))
       (old-val (sl-memory-read (-> self mem) addr))
       (bit-mask (byte-enable-to-bit-mask (-> self mem-wmask value sl-uint-value))))
  (sl-memory-write (-> self mem) addr
    (logior (logand old-val (lognot bit-mask))
            (logand (-> self mem-wdata value sl-uint-value) bit-mask))))
```

## The Assembler

The RV32I assembler (`rv32i-asm.lisp`) translates S-expression assembly into
32-bit machine code. It maintains an `rv32i-asm-state` object containing:

- **symtab** -- Symbol table mapping register names (x0-x31, t0-t6, a0-a7,
  s0-s11, ra, sp, etc.) and user-defined labels to numeric values
- **prog-mem** -- Assembled instruction words (loaded at address `pc0`)
- **data-mem** -- Data words (loaded at address `data0`)
- **instr-count** -- Current instruction count for label address computation

### Supported Instructions

All RV32I base integer instructions:

| Type | Instructions |
|------|-------------|
| R-type | `add`, `sub`, `sll`, `slt`, `sltu`, `xor`, `srl`, `sra`, `or`, `and` |
| I-type | `addi`, `slti`, `sltiu`, `xori`, `ori`, `andi`, `jalr`, `lb`, `lh`, `lw`, `lbu`, `lhu` |
| I-shift | `slli`, `srli`, `srai` |
| S-type | `sb`, `sh`, `sw` |
| B-type | `beq`, `bne`, `blt`, `bge`, `bltu`, `bgeu` |
| U-type | `lui`, `auipc` |
| J-type | `jal` |

Pseudoinstructions:

| Pseudo | Expansion |
|--------|-----------|
| `(nop)` | `(addi x0 x0 0)` |
| `(mv rd rs)` | `(addi rd rs 0)` |
| `(li rd imm)` | `(addi rd x0 imm)` for small immediates, `(lui rd upper)` + `(addi rd rd lower)` for large |
| `(j offset)` | `(jal x0 offset)` |
| `(ret)` | `(jalr x0 ra 0)` |

### Labels

Labels are declared with `(label name)` and can be used as branch/jump
targets. The assembler stores the label's byte address in the symbol table
and automatically computes PC-relative offsets for B-type and J-type
instructions:

```lisp
;; Loop that computes 1+2+3+4+5 = 15
'((li t0 0)          ; accumulator
  (li t1 1)          ; counter
  (li t2 6)          ; limit
  (label loop)
  (add t0 t0 t1)     ; acc += counter
  (addi t1 t1 1)     ; counter++
  (bne t1 t2 loop)   ; branch back if counter != limit
  (nop) (nop))
```

### Variables

Data variables can be declared with `(var type name &optional init-value)`:

```lisp
'((var u32 my-data 42)    ; 32-bit variable initialized to 42
  (lw t0 x0 my-data))     ; load variable into t0 (via address in symtab)
```

### How Programs Are Loaded into Memory

After assembling a program, `load-program-into-dram` writes the assembled
instructions and data into the DRAM's memory array before simulation starts:

1. **Program instructions** are written starting at word address 0
   (byte address `pc0`, default 0)
2. **Data words** are written starting at word address `data0/4`
   (byte address `data0`, default `#x800000`)

```
  DRAM Memory Map
  ┌──────────────────────┐ 0x000000
  │  Instruction 0       │
  │  Instruction 1       │
  │  ...                 │
  │  Instruction N       │
  ├──────────────────────┤
  │  (unused)            │
  ├──────────────────────┤ 0x800000 (data0)
  │  Data word 0         │
  │  Data word 1         │
  │  ...                 │
  └──────────────────────┘
```

## Running a Program

### Basic Usage

```lisp
(in-package :system-lisp-examples)

;; Define a program as a list of S-expression instructions
(defparameter *my-program*
  '((li t0 1)            ; t0 = 1
    (li t1 2)            ; t1 = 2
    (add t2 t0 t1)       ; t2 = t0 + t1 = 3
    (slli t3 t2 4)       ; t3 = t2 << 4 = 48
    (nop) (nop) (nop)))

;; Run with a simulation time limit (in time units)
(run-femtorv32-dram-tb *my-program* 500)
```

The simulation produces a `waves.vcd` file that can be opened in GTKWave or
any VCD viewer.

### Checking Results

Pass a finalize lambda to inspect register values after the simulation:

```lisp
(run-femtorv32-dram-tb
  '((li t0 7)
    (li t1 3)
    (add t2 t0 t1)
    (nop) (nop) (nop))
  500
  (lambda ()
    (format t "t0 = ~a~%" (get-cpu-regval 't0))  ; 7
    (format t "t1 = ~a~%" (get-cpu-regval 't1))  ; 3
    (format t "t2 = ~a~%" (get-cpu-regval 't2)))) ; 10
```

### Writing Tests with FiveAM

The test suite in `femtorv32-prog-test.lisp` shows the recommended pattern:

```lisp
(fiveam:test my-test
  (let ((test-prog '((li t0 0)
                     (li t1 1)
                     (li t2 6)
                     (label loop)
                     (add t0 t0 t1)
                     (addi t1 t1 1)
                     (bne t1 t2 loop)
                     (nop) (nop))))
    (run-femtorv32-dram-tb
      test-prog 2000
      (lambda ()
        (fiveam:is (sl= 15 (get-cpu-regval 't0)))
        (fiveam:is (sl= 6  (get-cpu-regval 't1)))))))
```

Use `500` sim time for simple single-instruction tests, `1000` for
load/store and shift operations (which require extra FSM cycles), and `2000`
or more for programs with loops.

### Running the Test Suite

From the REPL after loading the system:

```lisp
(ql:quickload "system-lisp-examples")
(fiveam:run! 'sl-examples::femtorv32-prog-tests)
```

## Future Work

- **More program examples** -- Sorting algorithms, Fibonacci, matrix
  operations, and other classic programs to exercise the full instruction set
- **Full RV32I support** -- The current implementation covers the base
  integer instructions but does not yet implement `fence`, `ecall`, and
  `ebreak`
- **Compilers for higher-level languages** -- Implement compilers for simple
  languages with Lisp-like syntax that target the RV32I assembler, taking
  advantage of Common Lisp's macro system for metaprogramming at the
  compiler level
- **RV32M/RV32F extensions** -- Add multiply/divide and floating-point
  instruction support to both the CPU model and the assembler
- **Peripherals** -- UART, GPIO, timer, and interrupt controller models to
  build a more complete SoC simulation
- **Performance optimization** -- Profile and optimize the simulation kernel
  for larger programs and longer simulation runs
