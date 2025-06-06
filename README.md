```markdown
# Causal Inference with Error-Prone Variables

This is a self-contained tutorial on how to adjust for **measurement error** in causal inference settings, especially when treatment or observed confounder variables are error-prone.

We demonstrate:

- What happens to causal effect estimation when measurement error is ignored
- Why this leads to biased estimates (often attenuation)
- Two methods for correction:
  - **Regression Calibration** (a classical approach)
  - **Control Variates** (a modern and flexible strategy)

The tutorial includes step-by-step R simulations, visualizations, and code explanations to illustrate each concept.

---

## 📖 Live Tutorial

You can view the tutorial as a rendered HTML page here:  
👉 **[View Tutorial](https://jehoonchae.github.io/eiv-tutorial/)**  

---

## 📁 Project Structure

```
.
├── tutorial.qmd           # Main Quarto tutorial
├── _quarto.yml            # Quarto configuration file
├── refs/                  # Bibliography and citation style files
│   ├── references.bib
│   └── chicago-author-date.csl
├── styles/                # Custom SCSS for styling
│   └── custom.scss
├── data/                  # Sample datasets
├── code/                  # Helper R scripts
├── images/                # Plots or static figures
├── docs/                  # Rendered HTML output for GitHub Pages
├── LICENSE
└── README.md              # You're here
```

---

## 🧠 Background

This tutorial was created as the final project for **Stats 256: Causal Inference**, taught by Professor [Chad Hazlett](https://www.chadhazlett.com/) at **UCLA** in **Spring 2025**.

**Authors:**
- [Je Hoon Chae](https://jehoonchae.github.io/)
- Daniela R. Amaya

---

## 🛠 Requirements

To run or modify this tutorial locally, you will need:

- [R](https://www.r-project.org/)
- [Quarto](https://quarto.org/)
- R packages such as `ggplot2`, `dplyr`, `tidyr`, etc. (see code chunks)

Render locally with:

```r
quarto::render("tutorial.qmd")
```

---

## 📄 License

This project is licensed under the [MIT License](LICENSE).
```