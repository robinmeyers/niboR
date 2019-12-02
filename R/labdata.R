
#' run sample quantification on plate reader data from BCA kit
#'
#' Specify standards with "Standard" in Sample columns
#' and the concentrations in the Conc column
#'
#' @param f a csv file with header row including Sample, Value, and Conc columns
#' @param dilution_factor dilution factor of test samples
#' @param target_amt ug of sample to load in a gel
#' @param target_vol ul to load
#' @return  list with a dataframe and a ggplot object
#' @export
bca_quant <- function(f, dilution_factor=5, target_amt=20, target_vol=15) {

    dat <- read_csv(f)

    if (! all(c("Sample", "Value", "Conc") %in% colnames(dat))) {
        stop("CSV header does not contain required fields")
    }

    if (sum(dat$Sample == "Standard") < 3) {
        stop("Not enough Standard entries Sample column in CSV")
    }

    standard_lm <- dat %>%
        filter(Sample == "Standard") %>%
        lm(Conc ~ Value, data = .)

    samples <- dat %>%
        filter(Sample != "Standard") %>%
        group_by(Sample) %>%
        mutate(RepValue = Value,
               Value = mean(Value)) %>%
        ungroup() %>%
        mutate(Conc = predict(standard_lm, newdata = .))

    loading_calc <-
        samples %>%
        distinct(Sample, Conc) %>%
        mutate(DilutedConc = round(Conc, 2),
               Concentration = DilutedConc*dilution_factor,
               Target = target_amt,
               VolSample = (target_amt/Conc) %>% round(2),
               VolBuffer = (target_vol-VolSample) %>% round(2))

    fit_plot <- standard_lm$model %>%
        ggplot(aes(x = Conc, y = Value)) +
        geom_point(color = "black") +
        geom_abline(slope = 1/coef(standard_lm)[2],
                    intercept = -coef(standard_lm)[1]/coef(standard_lm)[2], color = "black") +
        # geom_smooth(method = "lm", color = "black", se = F) +
        geom_point(data = samples,
                   aes(y = RepValue, color = Sample)) +
        scale_color_brewer(palette = "Set1")

    results <- loading_calc %>%
        select(Sample, DilutedConc, Concentration, Target, VolSample, VolBuffer)

    return(list(loading_calc=results, fit_plot=fit_plot))

}
