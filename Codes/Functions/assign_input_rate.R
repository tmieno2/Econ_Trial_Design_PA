
assign_input_rate <- function(N_levels, block_num, design) {
    
    # --------------
    # Strip designs
    # --------------
    
    if (design == "Strip Completely Random") {
        
        N_design <- data.table(
            #* each plot is a block by itself
            block_id = 1:block_num,
            plot_in_block_id = 1,
            N = sample(rep(N_levels, block_num / length(N_levels)), 
                       size = block_num, replace = FALSE)
        )
        
    } else if (design == "Strip Randomized Block") {
        
        N_design <- data.table(
            block_id = rep(1:block_num, each = 6),
            plot_in_block_id = rep(1:6, block_num),
            N = replicate(block_num, sample(N_levels, replace = FALSE)) %>% as.vector()
        )
        
    } else if (design == "Strip Fixed Block Best") {
        
        D.matrix <- readRDS(here("Shared/Results/strip_matrix_best.rds"))
        N_design <- data.table(
            block_id = rep(1:block_num, each = 6),
            plot_in_block_id = rep(1:6, block_num),
            N = N_levels[rep(c(D.matrix), times = block_num)]
        )
        
    } else if (design == "Strip Fixed Block Worst") {
        
        D.matrix <- readRDS(here("Shared/Results/strip_matrix_worst.rds"))
        #===Note: It's actually the double-strip-block design c(1:6,6:1)
        N_design <- data.table(
            block_id = rep(1:block_num, each = 12),
            plot_in_block_id = rep(1:12, block_num),
            N = N_levels[rep(c(D.matrix, D.matrix %>% rev()), times = block_num)]
        )
        


    # ----------------
    # Gridded designs
    # ----------------

    } else if (design == "Completely Random") {
        
        N_design <- data.table(
            #* each plot is a block by itself
            block_id = 1:block_num,
            plot_in_block_id = 1,
            N = sample(rep(N_levels, block_num / length(N_levels)), 
                       size = block_num, replace = FALSE)
        )
        
    } else if (design == "Randomized Block") {
        
        N_design <- data.table(
            block_id = rep(1:block_num, each = 6),
            plot_in_block_id = rep(1:6, block_num),
            N = replicate(block_num, sample(N_levels, replace = FALSE)) %>% as.vector()
        )
        
    } else if (design == "Fixed Block Best") {

        D.matrix <- readRDS(here("Shared/Results/FB_matrix_best.rds"))
        N_design <- data.table(
            block_id = rep(1:block_num, each = 6),
            plot_in_block_id = rep(1:6, block_num),
            N = N_levels[rep(c(t(D.matrix)), times = block_num)]
        )

    } else if (design == "Fixed Block Worst") {
        
        D.matrix <- readRDS(here("Shared/Results/FB_matrix_worst.rds"))
        N_design <- data.table(
            block_id = rep(1:block_num, each = 6),
            plot_in_block_id = rep(1:6, block_num),
            N = N_levels[rep(c(t(D.matrix)), times = block_num)]
        )
        
    } else if (design == "Latin Square Best") {
        
        D.matrix <- readRDS(here("Shared/Results/latin_matrix_best.rds"))
        N_design <- data.table(
            block_id = rep(1:block_num, each = 36),
            plot_in_block_id = rep(1:36, block_num),
            N = N_levels[rep(c(t(D.matrix)), times = block_num)]
        )
        
    } else if (design == "Latin Square Worst") {
        
        D.matrix <- readRDS(here("Shared/Results/latin_matrix_worst.rds"))
        N_design <- data.table(
            block_id = rep(1:block_num, each = 36),
            plot_in_block_id = rep(1:36, block_num),
            N = N_levels[rep(c(t(D.matrix)), times = block_num)]
        )
        
    } else if (design == "Latin Square LimJump") {

        D.matrix <- readRDS(here("Shared/Results/LJ_matrix_best.rds"))
        N_design <- data.table(
            block_id = rep(1:block_num, each = 36),
            plot_in_block_id = rep(1:36, block_num),
            N = N_levels[rep(c(t(D.matrix)), times = block_num)]
        )
        
    # ----------------
    # other designs
    # ----------------    

    } else if (design == "Cascade Plot") {
        
        M.cascade <- rbind(
            c(1:6, 6:1),
            c(2:6, 6:1, 1:1),
            c(3:6, 6:1, 1:2),
            c(4:6, 6:1, 1:3),
            c(5:6, 6:1, 1:4),
            c(6:6, 6:1, 1:5),
            c(6:1, 1:6),
            c(5:1, 1:6, 6:6),
            c(4:1, 1:6, 6:5),
            c(3:1, 1:6, 6:4),
            c(2:1, 1:6, 6:3),
            c(1:1, 1:6, 6:2)
        )
        N_design <- data.table(
            block_id = rep(1:block_num, each = 144),
            plot_in_block_id = rep(1:144, block_num),
            N = N_levels[rep(c(t(M.cascade)), times = block_num)]
        )
    } else if (design == "Wave") {
        
        M.wave <- rbind(
            c(4, 4, 4, 4, 4, 3, 3, 3, 3, 3),
            c(4, 5, 5, 5, 4, 3, 2, 2, 2, 3),
            c(4, 5, 6, 5, 4, 3, 2, 1, 2, 3),
            c(4, 5, 5, 5, 4, 3, 2, 2, 2, 3),
            c(4, 4, 4, 4, 4, 3, 3, 3, 3, 3),
            c(3, 3, 3, 3, 3, 4, 4, 4, 4, 4),
            c(3, 2, 2, 2, 3, 4, 5, 5, 5, 4),
            c(3, 2, 1, 2, 3, 4, 5, 6, 5, 4),
            c(3, 2, 2, 2, 3, 4, 5, 5, 5, 4),
            c(3, 3, 3, 3, 3, 4, 4, 4, 4, 4)
        )
        N_design <- data.table(
            block_id = rep(1:block_num, each = 100),
            plot_in_block_id = rep(1:100, block_num),
            N = N_levels[rep(c(t(M.wave)), times = block_num)]
        )
        
    } else {
        N_design <- NA
    }
    
    return(N_design)
}