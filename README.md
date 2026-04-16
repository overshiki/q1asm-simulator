# q1asm-simulator

A **Haskell-based simulator** for the Qblox Q1 Sequence Processor. It faithfully models the dual-core architecture—**Q1 Core** (classical control) and **RT Core** (hard real-time execution)—connected by a 32-deep FIFO queue, and executes Q1ASM programs with deterministic timing.

---

## Background

Quantum control hardware demands nanosecond-precision timing, deterministic execution, and low-latency feedback. The **Qblox Q1 Sequence Processor** addresses this with a heterogeneous dual-core design: a classical **Q1 Core** that handles loops, arithmetic, and program flow, and a hard real-time **RT Core** that drives AWGs, ADCs, and NCOs with zero-jitter instruction timing. The two cores communicate through a shallow 32-deep FIFO queue, creating a unique programming model where the Q1 Core *pushes* real-time instructions ahead of the RT Core's consumption.

This simulator reproduces that exact execution model in Haskell. It is intended for:

- **Offline validation** of Q1ASM sequences before hardware upload.
- **Pedagogical exploration** of the Q1 Core / RT Core interaction, queue underflow risks, and latched-parameter semantics.
- **Rapid prototyping** of new instructions and conditional-feedback patterns.

The project provides both a **batch simulator** (fast, concurrent execution) and an **interactive REPL** (cycle-accurate stepping of both cores and the queue).

---

## Features

- **Dual-core simulation**: Q1 Core handles loops, arithmetic, and jumps; RT Core drives waveforms and acquisitions with nanosecond precision.
- **Conditional execution**: Supports `set_cond` + LINQ-style trigger mocking, enabling active-reset and conditional-pulse simulation.
- **Latch/commit semantics**: `set_freq`, `set_phase`, `set_awg_offs`, and `set_awg_gain` are held in a latched state and atomically committed by `play`, `acquire`, or `upd_param`.
- **NCO phase coherence**: Phase accumulates continuously across `wait`, `wait_sync`, `acquire`, and other timed instructions.
- **Deterministic underflow detection**: Detects when the RT Core exhausts the queue before the Q1 Core can refill it (`RtUnderflow`).
- **Duration validation**: Enforces hardware constraints on all RT instructions—multiples of 4 ns, minimum 4 ns, maximum 65,535 ns.
- **Weighted integration**: `acquire_weighed` correctly applies weight tables and stores complex integrated results.
- **Native binary instruction encoding**: RT instructions crossing the queue are encoded as little-endian `RtPacket`s (64-bit header + optional 32-bit extended words).
- **Waveform rendering**: `play` instructions lookup waveform tables and render I/Q output samples using NCO phase accumulation, with gain/offset applied.
- **JSON sequence input**: Loads standard Qblox-style sequence dictionaries.

---

## Build

Requires **GHC 9.6+** and **cabal-install 3.10+**.

```bash
cabal build
```

The project provides two executables:
- **`q1asm-simulator`** — batch runner (simulates a JSON sequence in one shot).
- **`q1asm-repl`** — interactive dual-core debugger (step Q1 Core and RT Core one instruction at a time).

To run the batch simulator directly via cabal:

```bash
cabal run q1asm-simulator -- <sequence.json>
```

To start the interactive REPL:

```bash
cabal run q1asm-repl
```

---

## CLI Usage

The simulator takes a single argument: a **JSON sequence dictionary**.

```bash
q1asm-simulator my_experiment.json
```

### Sequence Dictionary Format

The JSON file follows the Qblox sequence dictionary structure:

```json
{
  "waveforms": {
    "0": [0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5]
  },
  "weights": {
    "0": [1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0],
    "1": [0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0]
  },
  "acquisitions": {
    "0": { "num_bins": 10 }
  },
  "program": [
    ["", "move", "5, R0", ""],
    ["start", "play", "0,0,20", ""],
    ["", "wait", "100", ""],
    ["", "loop", "R0, @start", ""],
    ["", "stop", "", ""]
  ]
}
```

| Field | Description |
|-------|-------------|
| `waveforms` | Map of waveform index → list of float samples (1 ns/sample). |
| `weights` | Map of weight index → list of float samples (for `acquire_weighed`). |
| `acquisitions` | Map of acquisition index → `{ "num_bins": N }`. |
| `program` | List of 4-tuples: `(label, mnemonic, args, comment)`. |

### Example: Run a Simple Loop

Create `test_sequence.json`:

```json
{
  "waveforms": {
    "0": [0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5]
  },
  "weights": {},
  "acquisitions": {},
  "program": [
    ["", "move", "5, R0", ""],
    ["start", "play", "0,0,20", ""],
    ["", "wait", "100", ""],
    ["", "loop", "R0, @start", ""],
    ["", "stop", "", ""]
  ]
}
```

Run it:

```bash
cabal run q1asm-simulator -- test_sequence.json
```

Expected output:

```
Simulation completed successfully.
Q1 Core final PC: 4
Q1 Core cycles: 88
RT Core time: 600 ns
Output samples (I): 100
Output samples (Q): 100
Acquisitions: fromList []
```

This program plays a 20 ns pulse, waits 100 ns, and repeats 5 times, yielding exactly **600 ns** of RT time and **100** rendered samples.

---

## Interactive REPL (`q1asm-repl`)

The REPL is a cycle-accurate debugger. You can load a sequence and step through the Q1 Core and RT Core one instruction at a time, watching how the 32-deep FIFO queue mediates between them.

### Starting the REPL

```bash
cabal run q1asm-repl
```

### REPL Commands

| Command | Alias | Description |
|---------|-------|-------------|
| `load <file>` | — | Load a JSON sequence dictionary. Resets all state. |
| `next` | `n` | Execute one instruction (auto-scheduled). |
| `next q1` | `nq1` | Force-execute one Q1 instruction. |
| `next rt` | `nrt` | Force-execute one RT instruction. |
| `continue` | `c` | Run `next` repeatedly until halt or error. |
| `info q1` | `i q1` | Print PC, registers, and Q1 cycle time. |
| `info rt` | `i rt` | Print RT time, latched params, NCO, and output length. |
| `info queue` | `i q` | Print queue depth and pending instructions. |
| `info bins` | `i b` | Print acquisition bins. |
| `info waveforms` | `i w` | List loaded waveform indices and lengths. |
| `reset` | `r` | Reset simulation to initial state. |
| `help` | `h` | Show command help. |
| `quit` | `q` | Exit REPL. |

### How Stepping Works

The REPL uses an **earliest-event scheduler**:

- If the Q1 Core is ready before the RT Core (`t_q1 < t_rt`), you see a **Q1-only** step with a note that the RT Core is busy.
- If the RT Core is ready before the Q1 Core (`t_rt < t_q1`), you see an **RT-only** step with a note that Q1 is ahead.
- If both are ready at the exact same time (`t_q1 == t_rt`), you see a **SIMUL** step showing both cores acting together.

### Example REPL Session

```text
$ cabal run q1asm-repl
Q1ASM Simulator REPL
Type 'help' for commands, 'quit' to exit.
q1asm-repl> load test_sequence.json
Loaded: test_sequence.json

q1asm-repl> n
────────────────────────────────────────────
Step 1 | Q1 | t_q1=0ns → 4ns
       Move (Imm 5) 0
       PC: 0 → 1
       R0: 0 → 5
────────────────────────────────────────────

q1asm-repl> n
────────────────────────────────────────────
Step 2 | Q1 | t_q1=4ns → 8ns
       queue push: Play (Imm 0) (Imm 0) 20
       queue depth: 0 → 1
────────────────────────────────────────────

q1asm-repl> n
────────────────────────────────────────────
Step 3 | RT | t_rt=0ns → 20ns | Q1 ahead at 8ns
       Play (Imm 0) (Imm 0) 20
       RT time: 0ns → 20ns
       Output: +20 samples (I/Q)
       queue pop | depth: 1 → 0
────────────────────────────────────────────

q1asm-repl> n
────────────────────────────────────────────
Step 4 | Q1 | t_q1=8ns → 12ns | RT busy until 20ns
       queue push: Wait 100
       queue depth: 0 → 1
────────────────────────────────────────────

q1asm-repl> n
────────────────────────────────────────────
Step 5 | SIMUL | t=20ns
[Q1]   queue push: Play (Imm 0) (Imm 0) 20
[RT]   Wait 100
       RT time: 20ns → 120ns
       queue net | depth: 1 → 1
────────────────────────────────────────────

q1asm-repl> c
Running...
Simulation halted.
q1asm-repl> i rt
RT Core State:
  Time:         600 ns
  Running:      True
  Output I:     100 samples
  Output Q:     100 samples

q1asm-repl> q
Goodbye.
```

---

## Supported Q1ASM Instructions

### Q1 Core (Classical)

| Instruction | Example | Description |
|-------------|---------|-------------|
| `move` | `move 10, R0` | Load immediate or register into `Rd`. |
| `add` | `add R0, R1, R2` | `R2 = R0 + R1` (mod 2³²). |
| `sub` | `sub R0, R1, R2` | `R2 = R0 - R1` (mod 2³²). |
| `and` | `and R0, R1, R2` | `R2 = R0 & R1`. |
| `or` | `or R0, R1, R2` | `R2 = R0 \| R1`. |
| `xor` | `xor R0, R1, R2` | `R2 = R0 ^ R1`. |
| `not` | `not R0, R1` | `R1 = ~R0`. |
| `asl` | `asl R0, 2, R1` | `R1 = R0 << 2`. |
| `asr` | `asr R0, 2, R1` | `R1 = R0 >> 2`. |
| `jmp` | `jmp @target` | Absolute jump to label. |
| `jge` | `jge R0, R1, @target` | Jump if `R0 >= R1`. |
| `jlt` | `jlt R0, R1, @target` | Jump if `R0 < R1`. |
| `loop` | `loop R0, @start` | Decrement `R0`; jump to label if non-zero. |
| `jr` | `jr -2` | Relative jump by offset. |
| `nop` | `nop` | No operation. |
| `stop` / `halt` | `stop` | Halt the sequencer. |

### RT Core (Real-Time)

| Instruction | Example | Description |
|-------------|---------|-------------|
| `wait` | `wait 100` | Pause RT timeline for 100 ns. |
| `wait_sync` | `wait_sync 200` | Synchronization barrier (single-seq mode behaves like `wait`). |
| `play` | `play 0,0,20` | Play waveform 0 on both paths for 20 ns. |
| `acquire` | `acquire 0,0,120` | Trigger acquisition into bin 0 for 120 ns. |
| `acquire_weighed` | `acquire_weighed 0,0,120,0,1` | Weighted integration using weights 0 and 1. |
| `acquire_ttl` | `acquire_ttl 0,0,1,120` | TTL trigger acquisition. |
| `set_freq` | `set_freq 100,0` | Latch NCO frequency update. |
| `set_phase` | `set_phase 90,0` | Latch NCO phase update. |
| `reset_phase` | `reset_phase` | Reset NCO phase to 0. |
| `set_awg_offs` | `set_awg_offs 0.5,0.0` | Latch AWG I/Q offset. |
| `set_awg_gain` | `set_awg_gain 32767,32767` | Latch AWG I/Q gain. |
| `upd_param` | `upd_param 4` | Atomically commit latched values, then wait 4 ns. |
| `set_cond` | `set_cond 1,1,0,4` | Arm conditional execution for next RT instruction. |
| `latch_rst` | `latch_rst 4` | Reset trigger latch. |
| `set_latch_en` | `set_latch_en 1,4` | Enable/disable trigger latch. |
| `wait_trigger` | `wait_trigger 1,100` | Wait for trigger or timeout. |
| `set_scope_en` | `set_scope_en 1` | Latch scope enable. |
| `set_time_ref` | `set_time_ref` | Set reference timestamp (4 ns no-op in single-seq mode). |
| `set_digital` | `set_digital 1` | Set digital output (QTM no-op placeholder). |

---

## Library Usage

You can also use the simulator as a Haskell library:

```haskell
import qualified Data.Vector as V
import Q1Asm.Simulator (runSimulation)
import Q1Asm.Types

main :: IO ()
main = do
  let waveforms = V.singleton (V.replicate 20 0.5)
      cfg = SimConfig
        { simModuleType     = Qcm
        , simWaveforms      = waveforms
        , simWeights        = V.empty
        , simMockData       = Nothing
        , simTriggerLatency = 212
        , simTriggers       = mempty
        }
      prog =
        [ Q1Only (Move (Imm 5) 0)
        , Rt (Play (Imm 0) (Imm 0) 20)
        , Rt (Wait 100)
        , Q1Only (Loop 0 1)
        , Q1Only Stop
        ]
  result <- runSimulation cfg prog
  print result
```

---

## Architecture Overview

```
┌─────────────┐      ┌─────────────┐      ┌─────────────────────┐
│   Parser    │─────▶│   Q1 Core   │─────▶│   32-deep FIFO      │
│  (Text/JSON)│      │  (StateM)   │      │   (STM TBQueue)     │
└─────────────┘      └─────────────┘      └─────────────────────┘
                                                     │
                                                     ▼
                                           ┌─────────────┐
                                           │   RT Core   │
                                           │  (StateM)   │
                                           └─────────────┘
                                                 │
                     ┌───────────────────────────┼───────────┐
                     ▼                           ▼           ▼
               ┌──────────┐              ┌─────────────┐  ┌────────┐
               │  NCO/AWG │              │ Acquisition │  │ Output │
               │ Renderer │              │    Bins     │  │  Sink  │
               └──────────┘              └─────────────┘  └────────┘
```

---

## Notes & Limitations

- **Single-sequencer mode**: Multi-sequencer SYNQ/LINQ cluster support is stubbed in `Q1Asm.Cluster` and not yet active.
- **QTM fidelity**: Time-tagging instructions (`acquire_timetags`, `acquire_digital`, `upd_thres`) are not yet implemented.
- **LINQ feedback instructions**: `fb_pop_data`, `fb_pull_data`, `fb_com_*`, `fb_acq_*`, etc. are not yet implemented.
- **Distortion correction**: Pre-distortion filters are not applied in this version.
- **Trigger behavior**: In single-sequencer mode, `wait_trigger` always times out unless mock triggers are supplied via `SimConfig`.

---

## License

MIT
