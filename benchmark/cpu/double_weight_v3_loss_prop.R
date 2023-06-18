devtools::load_all()

loss_props <- c(0, 0.5, 1)
for (loss_prop in loss_props) {
  print("prop: " %+% loss_prop)
  cpu_double_weight_v3_loss_prop <- run_benchmark(fpath = "../../data/small_data/cpu.csv",
                                                  y_col_idx = 1,
                                                  len_candidate_basis_set = 209,
                                                  len_final_basis_set = 209,
                                                  max_rows = 209,
                                                  max_degree = 6,
                                                  batch_size = 100,
                                                  n_batch = 100,
                                                  p = 0.5,
                                                  seed = 12941,
                                                  weight_function = "double weight v3",
                                                  loss_prop = loss_prop,
                                                  n_cores = 32)
  saveRDS(cpu_double_weight_v3_loss_prop, "cpu_double_weight_v3_" %+% loss_prop %+% ".RDS")
  gc()
}
