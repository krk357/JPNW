# Japan Women's Political Attitudes – Analysis of WVS Data

This project analyzes data from the **World Values Survey Wave 7 (2017–2022)** with a focus on **Japanese women's political preferences**, particularly how education, ideology, and other socio-economic factors relate to environmental vs. economic policy prioritization.

## Project Structure

```
.
├── Japan Women.R            # Main R analysis script
├── output/                  # Stargazer model outputs in HTML
├── JPNW.Rproj               # RStudio project file
├── .gitignore               # Files/folders excluded from version control
└── README.md                # This file
```

## Required R Packages

The script will automatically check and install the following packages:

- foreign
- dplyr
- car
- nnet
- summarytools
- stargazer

## Data

This project uses WVS Wave 7. The `.dta` file is not included in this repository for licensing reasons. Please download the dataset from:

[https://www.worldvaluessurvey.org/](https://www.worldvaluessurvey.org/)

## Models Estimated

- Binary logistic models on:
  - "Don't know" responses
  - Preference for protecting the environment
  - Preference for protecting the economy
- Multinomial logistic regression on full policy preference spectrum

## Output

Model results are saved to `output/` as HTML tables (created with `stargazer`). These include:

- `dunno_res.html`
- `pronatu_res.html`
- `proecon_res.html`
- `multimod_res.html`

## Author

**Ville Virkkunen**\
MSc Student in Political Science\
University of Turku

## License

This project is licensed under the **MIT License**. See the `LICENSE` file for details.

---

This project is exploratory. Feel free to fork, reuse or suggest improvements.

