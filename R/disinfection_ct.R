# CT Calculations

chemdose_ct <- function(water, time, residual, baffle, volume, flow) {

  ph <- water@ph
  temp <- water@temp

  if(missing(time) & !missing(volume) & !missing(flow)) {
    time <- volume / flow
  }

  if(temp < 12.5) {
    ct_required = (.353*.5)*(12.006 +exp(2.46-.073*temp+.125*residual+.389*ph))
    log_removal = (residual*time*baffle)/(12.006 +exp(2.46-.073*temp+.125*residual+.389*ph))*1/.353
  } else {
    ct_required <- (.361*0.5)*(-2.216 +exp(2.69-.065*temp+.111*residual+.361*ph))
    log_removal = (residual*time*baffle)/(-2.216 +exp(2.69-.065*temp+.111*residual+.361*ph)))/.361)
  }

  ct_actual = residual*time*baffle

  # What to actually return?

}

ozonate_ct <- function(water, time, residual) {
  # Log removal calc here.
}