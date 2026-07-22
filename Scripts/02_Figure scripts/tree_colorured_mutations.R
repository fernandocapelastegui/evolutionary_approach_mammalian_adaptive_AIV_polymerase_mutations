library(ape)
library(stringr)

# run the script that collects the EPI_ISL for sequences with mutation

source(file = "gitfilepath/git/evolutionary_approach_mammalian_adaptive_AIV_polymerase_mutations/Scripts/02_Figure scripts/mutation_lists_ids.R")

# FIle path configs

tree_dir   <- "gitfilepath/git/mutation_pathway/Data/Final/07_trees"
output_dir <- "gitfilepath/git/evolutionary_approach_mammalian_adaptive_AIV_polymerase_mutations/Figures/trees/"


segments <- list(
  NP  = list(groups = groups_np,  tree_path = file.path(tree_dir, "NP_trimal_cialigned_cleaned.fasta.treefile")),
  PB2 = list(groups = groups_pb2, tree_path = file.path(tree_dir, "PB2_trimal_cialigned_cleaned.fasta.treefile")),
  PB1 = list(groups = groups_pb1, tree_path = file.path(tree_dir, "PB1_trimal_cialigned_cleaned.fasta.treefile")),
  PA  = list(groups = groups_pa,  tree_path = file.path(tree_dir, "PA_trimal_cialigned_cleaned.fasta.treefile"))
)

# ---- Pruning: remove the longest branch (or the clade below it) ----
prune_longest_branch <- function(tree) {
  n    <- length(tree$tip.label)
  i    <- which.max(tree$edge.length)   # the single longest branch
  node <- tree$edge[i, 2]               # its child node
  
  if (node <= n) {
    # longest branch leads to a single tip
    drop_tips <- tree$tip.label[node]
  } else {
    # longest branch is internal -> drop the whole clade below it
    drop_tips <- extract.clade(tree, node)$tip.label
  }
  
  drop.tip(tree, drop_tips)
}

# ---- Helper: compute tip colors for a tree given a groups list ----
get_tip_colors <- function(tree, groups) {
  tip_color <- rep(NA, length(tree$tip.label))
  for (g in names(groups)) {
    ids <- str_extract(groups[[g]]$tips, "EPI_ISL_\\d+")
    ids <- ids[!is.na(ids)]
    pattern <- paste0("(?<![0-9])(", paste(ids, collapse = "|"), ")(?![0-9])")
    hits <- str_detect(tree$tip.label, pattern)
    tip_color[hits & is.na(tip_color)] <- groups[[g]]$color
    cat("  Group", g, "- matched", sum(hits), "tips\n")
  }
  tip_color
}

# ---- Helper: draw one tree (used by both single-plot and panel) ----
draw_tree <- function(tree, groups, title = NULL, legend_inset = -0.15,
                      jitter_frac = 0.05, point_cex = 0.8) {
  tip_color <- get_tip_colors(tree, groups)
  n <- length(tree$tip.label)
  plot(tree,
       show.tip.label = FALSE,
       no.margin      = FALSE,
       edge.color     = "grey70",
       edge.width     = 0.2,
       main           = title)
  last    <- get("last_plot.phylo", envir = .PlotPhyloEnv)
  x_range <- diff(range(last$xx[1:n], na.rm = TRUE))
  hl  <- which(!is.na(tip_color))
  n_h <- length(hl)
  # Jitter only outward (rightward) from each tip's actual x position
  set.seed(1)
  x_jit <- runif(n_h, 0, jitter_frac * x_range)
  points(last$xx[hl] + x_jit,
         last$yy[hl],
         pch = 19,
         col = adjustcolor(tip_color[hl], alpha.f = 0.6),
         cex = point_cex,
         xpd = NA)
  legend("topright",
         legend = names(groups),
         col    = sapply(groups, function(x) x$color),
         pch    = 19, pt.cex = 1.2, bty = "n",
         title  = "Mutation",
         inset  = c(legend_inset, 0),
         xpd    = NA)
}

# ---- Read trees once (so the panel doesn't re-read them) ----
trees <- lapply(segments, function(s) read.tree(s$tree_path))

# ---- Prune longest branch(es) from each tree ----
prune_iterations <- 1L   # set higher to strip multiple long branches per tree
if (prune_iterations > 0L) {
  for (seg in names(trees)) {
    for (k in seq_len(prune_iterations)) {
      before <- length(trees[[seg]]$tip.label)
      trees[[seg]] <- prune_longest_branch(trees[[seg]])
      after  <- length(trees[[seg]]$tip.label)
      cat("Pruned", seg, ":", before, "->", after, "tips\n")
    }
  }
}

# ---- Individual plots ----
for (seg in names(segments)) {
  cat("Plotting", seg, "...\n")
  out_file <- file.path(output_dir, paste0("tree_highlighted_", seg, ".png"))
  png(out_file, width = 2000, height = 3000, res = 300)
  par(mar = c(1, 1, 1, 8))   # top margin a bit larger for title
  draw_tree(trees[[seg]], segments[[seg]]$groups, title = seg, legend_inset = -0.10)
  dev.off()
}

# ---- Combined panel (2x2) ----
cat("Plotting combined panel...\n")
png(file.path(output_dir, "tree_highlighted_panel.png"),
    width = 6000, height = 6000, res = 300)
par(mfrow = c(2, 2), mar = c(1, 1, 1, 8),
    cex = 1, cex.main = 1.2)
for (seg in names(segments)) {
  draw_tree(trees[[seg]], segments[[seg]]$groups, title = seg, legend_inset = -0.10)
}
dev.off()

cat("Done.\n")
