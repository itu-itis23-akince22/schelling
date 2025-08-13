# Schelling Model Simulation

This project implements a computational simulation of **Thomas Schelling’s segregation model**, which demonstrates how small individual preferences can lead to large-scale patterns of segregation.

## 📌 Overview

The Schelling model is an agent-based simulation where agents (representing individuals) are placed on a grid.  
Each agent has a tolerance threshold for being surrounded by agents of a different type.  
If the local composition doesn’t meet this preference, the agent moves to a different empty location.

Even when individuals have only a mild preference for neighbors similar to themselves, the model often evolves into strongly segregated patterns.

## 🛠 Project Components

- **Haskell implementation (`haskell_schelling.hs`)**  
  Performs the simulation logic — reading the initial grid, applying the movement rules, and producing a new state.

- **Julia scripts**  
  - `schelling.jl`: Standalone Julia version of the model.  
  - `schelling_withhaskell.jl`: Runs the Haskell simulation from Julia.  
  - `plot_start_and_finish.jl`: Visualizes the initial and final states of the simulation.

- **Data files**  
  - `cells.txt`: Example initial configuration of the grid.

## 🔍 How It Works

1. **Input**  
   The simulation starts from a text file (`cells.txt`) describing the grid:
   - Each cell can be empty or occupied by an agent of type A or B.

2. **Simulation Rules**  
   - For each agent, check the proportion of neighbors that are of the same type.
   - If the proportion is below a set threshold (e.g., 30%), the agent moves to a random empty cell.
   - Repeat until all agents are satisfied or a maximum number of iterations is reached.

3. **Output**  
   - The updated grid is saved to an output file (`cells_out.txt`).
   - Julia can be used to visualize before-and-after states.

## 🎯 Purpose

This project is designed to:
- Demonstrate emergent behavior from simple rules.
- Show how programming languages like **Haskell** and **Julia** can be combined for computation and visualization.
- Provide a framework for experimenting with segregation dynamics.

---
### 1️⃣ Run with Haskell
# Inside the project folder
cabal update
cabal build
cabal run schelling -- cells.txt cells_out.txt
###2️⃣ Run with Julia only
julia schelling.jl
###3️⃣ Run Julia + Haskell integration
julia schelling_withhaskell.jl
###📊 Visualization
To plot simulation results:
julia plot_start_and_finish.jl
