using Plots

# Configuration
gridSize = 200
colorPalette = [:white, :red, :blue]  # 0: empty, 1: red, 2: blue

# Read a grid file into a 2D matrix
function read_grid(filename)
    data = parse.(Int, readlines(filename))
    return reshape(data, gridSize, gridSize)'
end

# Load both grids
start_grid = read_grid("cells.txt")
final_grid = read_grid("cells_out.txt")

# Plot them side by side
p1 = heatmap(start_grid, c=colorPalette, legend=false, title="Start Grid", aspect_ratio=1)
p2 = heatmap(final_grid, c=colorPalette, legend=false, title="Final Grid", aspect_ratio=1)

# Show side-by-side
plot(p1, p2, layout=(1,2), size=(1000, 500))
