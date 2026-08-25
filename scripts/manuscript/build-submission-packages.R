#!/usr/bin/env Rscript

args_full <- commandArgs(trailingOnly = FALSE)
file_arg <- args_full[grepl("^--file=", args_full)]
script_path <- if (length(file_arg) > 0L) {
  normalizePath(sub("^--file=", "", file_arg[[1]]))
} else {
  normalizePath("scripts/manuscript/build-submission-packages.R")
}
repo_root <- normalizePath(file.path(dirname(script_path), "..", ".."))

manuscript_dir <- file.path(repo_root, "writing", "manuscript")
aoas_dir <- file.path(repo_root, "writing", "aoas")
arxiv_dir <- file.path(repo_root, "writing", "arxiv")
arxiv_zip <- file.path(arxiv_dir, "upload.zip")
template_dir <- file.path(repo_root, "vendor", "ims-aoas")
figure_root <- file.path(repo_root, "results", "figures", "manuscript")
macro_path <- file.path(repo_root, "results", "tables", "manuscript", "inference-results.tex")

read_tex <- function(path) readLines(path, warn = FALSE, encoding = "UTF-8")

line_index <- function(lines, pattern, fixed = TRUE) {
  hits <- if (fixed) which(lines == pattern) else grep(pattern, lines)
  if (length(hits) != 1L) {
    stop("Expected exactly one match for '", pattern, "' in source; found ", length(hits), ".")
  }
  hits[[1L]]
}

write_tex <- function(lines, path) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  writeLines(lines, path, useBytes = TRUE)
}

replace_fixed <- function(lines, old, new) {
  gsub(old, new, lines, fixed = TRUE)
}

copy_file <- function(source, target) {
  dir.create(dirname(target), recursive = TRUE, showWarnings = FALSE)
  if (!file.copy(source, target, overwrite = TRUE, copy.mode = TRUE)) {
    stop("Could not copy ", source, " to ", target, ".")
  }
}

main_lines <- read_tex(file.path(manuscript_dir, "main.tex"))
supp_lines <- read_tex(file.path(manuscript_dir, "supp.tex"))
technical_lines <- read_tex(file.path(manuscript_dir, "technical-supplement.tex"))
macro_lines <- read_tex(macro_path)

abstract_start <- line_index(main_lines, "\\begin{abstract}")
abstract_end <- line_index(main_lines, "\\end{abstract}")
main_start <- line_index(main_lines, "\\section{Introduction}")
ack_start <- line_index(main_lines, "\\section*{Acknowledgments}")
supplement_note_start <- line_index(main_lines, "\\section*{Supplementary Material}")

abstract_lines <- main_lines[(abstract_start + 1L):(abstract_end - 1L)]
article_body <- main_lines[main_start:(ack_start - 1L)]
ack_lines <- main_lines[(ack_start + 1L):(supplement_note_start - 1L)]

supp_body_start <- line_index(supp_lines, "\\section{Numerical Benchmarks} \\label{sec:supp-benchmarks}")
supp_bib_start <- line_index(supp_lines, "\\bibliographystyle{apalike}")
appendix_body <- c(technical_lines, "", supp_lines[supp_body_start:(supp_bib_start - 1L)])

figure_paths <- c(
  "figures/distributions/sports_cdf.png",
  "figures/case_studies/nba_extreme_case_study.png",
  "figures/fixed_clock_calibration_envelope.png",
  "figures/nfl/pit.png",
  "figures/nba/pit.png",
  "figures/simulation/null_conservatism.png",
  "figures/nfl/fixed_clock_surface_linear.png",
  "figures/nba/fixed_clock_surface_linear.png",
  "figures/nfl/fixed_clock_surface_locf.png",
  "figures/nba/fixed_clock_surface_locf.png"
)

arxiv_figure_names <- setNames(
  c(
    "sports_cdf.png",
    "nba_extreme_case_study.png",
    "fixed_clock_calibration_envelope.png",
    "nfl_pit.png",
    "nba_pit.png",
    "null_conservatism.png",
    "nfl_fixed_clock_surface_linear.png",
    "nba_fixed_clock_surface_linear.png",
    "nfl_fixed_clock_surface_locf.png",
    "nba_fixed_clock_surface_locf.png"
  ),
  figure_paths
)

if (anyDuplicated(unname(arxiv_figure_names))) {
  stop("arXiv figure names must be unique after flattening.")
}

for (path in figure_paths) {
  source <- file.path(figure_root, path)
  if (!file.exists(source)) stop("Missing submission figure: ", source)
}

dir.create(aoas_dir, recursive = TRUE, showWarnings = FALSE)
unlink(file.path(aoas_dir, "figures"), recursive = TRUE)
for (name in c("article.tex", "article.pdf", "supplement.tex", "supplement.pdf", "references.bib")) {
  unlink(file.path(aoas_dir, name))
}
for (stem in c("article", "supplement")) {
  unlink(file.path(aoas_dir, paste0(stem, ".", c("aux", "bbl", "blg", "fdb_latexmk", "fls", "log", "out"))))
}
for (name in c("imsart.cls", "imsart.sty", "imsart-nameyear.bst", "LICENSE")) {
  copy_file(file.path(template_dir, name), file.path(aoas_dir, name))
}
copy_file(file.path(manuscript_dir, "references.bib"), file.path(aoas_dir, "references.bib"))
for (path in figure_paths) {
  copy_file(file.path(figure_root, path), file.path(aoas_dir, path))
}

aoas_article <- c(
  "\\documentclass[aoas]{imsart}",
  "",
  "\\RequirePackage{amsthm,amsmath,amsfonts,amssymb}",
  "\\RequirePackage[authoryear]{natbib}",
  "\\RequirePackage{graphicx}",
  "\\RequirePackage{placeins}",
  "\\RequirePackage[colorlinks,citecolor=blue,urlcolor=blue,linkcolor=blue]{hyperref}",
  "",
  "\\startlocaldefs",
  "\\theoremstyle{plain}",
  "\\newtheorem{theorem}{Theorem}",
  "\\newtheorem{lemma}[theorem]{Lemma}",
  "\\newtheorem{proposition}[theorem]{Proposition}",
  "\\newtheorem{corollary}[theorem]{Corollary}",
  "\\theoremstyle{definition}",
  "\\newtheorem{definition}[theorem]{Definition}",
  "\\newtheorem{remark}[theorem]{Remark}",
  "\\newcommand{\\E}{\\mathbb{E}}",
  "\\renewcommand{\\P}{\\mathbb{P}}",
  "\\newcommand{\\V}{\\operatorname{Var}}",
  macro_lines,
  "\\endlocaldefs",
  "",
  "\\begin{document}",
  "\\begin{frontmatter}",
  "\\title{The Blown Lead Paradox: A Pathwise Calibration Benchmark for Win Probability Forecasts}",
  "\\runtitle{The Blown Lead Paradox}",
  "",
  "\\begin{aug}",
  "\\author[A]{\\fnms{Jonathan}~\\snm{Pipping-Gam\\'on}\\ead[label=e1]{jpipping@wharton.upenn.edu}\\orcid{0009-0000-2540-2469}}",
  "\\and",
  "\\author[A]{\\fnms{Abraham J.}~\\snm{Wyner}\\ead[label=e2]{ajw@wharton.upenn.edu}}",
  "\\address[A]{Department of Statistics, University of Pennsylvania \\printead[presep={,\\ }]{e1,e2}}",
  "\\end{aug}",
  "",
  "\\begin{abstract}",
  abstract_lines,
  "\\end{abstract}",
  "",
  "\\begin{keyword}",
  "\\raggedright",
  "\\kwd{\\mbox{Forecast calibration}}",
  "\\kwd{martingales}",
  "\\kwd[,\\newline]{\\mbox{pigeonhole bootstrap}}",
  "\\kwd{\\mbox{sports analytics}}",
  "\\kwd{\\mbox{win probability}}",
  "\\end{keyword}",
  "\\end{frontmatter}",
  "",
  article_body,
  "",
  "\\begin{acks}[Acknowledgments]",
  ack_lines,
  "\\end{acks}",
  "",
  "\\begin{supplement}",
  "\\stitle{Supplementary Material for The Blown Lead Paradox}",
  "\\sdescription{Proofs, dependence and bootstrap justification, full data and calibration documentation, robustness analyses, and finite-schedule validation.}",
  "\\end{supplement}",
  "",
  "\\bibliographystyle{imsart-nameyear}",
  "\\bibliography{references}",
  "\\end{document}"
)
write_tex(aoas_article, file.path(aoas_dir, "article.tex"))

aoas_supplement <- c(
  "\\documentclass[aoas]{imsart}",
  "",
  "\\RequirePackage{amsthm,amsmath,amsfonts,amssymb}",
  "\\RequirePackage[authoryear]{natbib}",
  "\\RequirePackage{graphicx}",
  "\\RequirePackage{placeins}",
  "\\RequirePackage{enumitem}",
  "\\RequirePackage[colorlinks,citecolor=blue,urlcolor=blue,linkcolor=blue]{hyperref}",
  "",
  "\\startlocaldefs",
  "\\renewcommand{\\P}{\\mathbb{P}}",
  "\\newcommand{\\E}{\\mathbb{E}}",
  macro_lines,
  "\\theoremstyle{plain}",
  "\\newtheorem{theorem}{Theorem}[section]",
  "\\newtheorem{proposition}[theorem]{Proposition}",
  "\\newtheorem{corollary}[theorem]{Corollary}",
  "\\renewcommand{\\thesection}{S\\arabic{section}}",
  "\\renewcommand{\\thesubsection}{S\\arabic{section}.\\arabic{subsection}}",
  "\\counterwithin{table}{section}",
  "\\counterwithin{figure}{section}",
  "\\renewcommand{\\thetable}{\\thesection.\\arabic{table}}",
  "\\renewcommand{\\thefigure}{\\thesection.\\arabic{figure}}",
  "\\endlocaldefs",
  "",
  "\\begin{document}",
  "\\begin{frontmatter}",
  "\\title{Supplementary Material for The Blown Lead Paradox: A Pathwise Calibration Benchmark for Win Probability Forecasts}",
  "\\runtitle{Supplementary Material for The Blown Lead Paradox}",
  "",
  "\\begin{aug}",
  "\\author[A]{\\fnms{Jonathan}~\\snm{Pipping-Gam\\'on}\\ead[label=e1]{jpipping@wharton.upenn.edu}\\orcid{0009-0000-2540-2469}}",
  "\\and",
  "\\author[A]{\\fnms{Abraham J.}~\\snm{Wyner}\\ead[label=e2]{ajw@wharton.upenn.edu}}",
  "\\address[A]{Department of Statistics, University of Pennsylvania \\printead[presep={,\\ }]{e1,e2}}",
  "\\end{aug}",
  "\\end{frontmatter}",
  "",
  "\\setcounter{section}{0}",
  appendix_body,
  "\\bibliographystyle{imsart-nameyear}",
  "\\bibliography{references}",
  "\\end{document}"
)
write_tex(aoas_supplement, file.path(aoas_dir, "supplement.tex"))

dir.create(arxiv_dir, recursive = TRUE, showWarnings = FALSE)
unlink(list.files(arxiv_dir, full.names = TRUE, all.files = TRUE, no.. = TRUE), recursive = TRUE)
unlink(file.path(repo_root, "writing", c("arxiv-upload.zip", "arxiv-preview.pdf")))
copy_file(file.path(manuscript_dir, "references.bib"), file.path(arxiv_dir, "references.bib"))
for (path in figure_paths) {
  copy_file(file.path(figure_root, path), file.path(arxiv_dir, path))
}

arxiv_body <- article_body
arxiv_body <- replace_fixed(
  arxiv_body,
  "Proofs and discrete-time corrections are given in Supplementary Sections~S1--S3.",
  "Proofs and discrete-time corrections are given in Appendices~\\ref{app:discrete-proofs}--\\ref{app:two-player-derivation}."
)
arxiv_body <- replace_fixed(
  arxiv_body,
  "; Supplementary Section~S3 gives the full derivation.",
  "; Appendix~\\ref{app:two-player-derivation} gives the full derivation."
)
arxiv_body <- replace_fixed(arxiv_body, "Supplementary Section~S4", "Appendix~\\ref{sec:supp-benchmarks}")
arxiv_body <- replace_fixed(
  arxiv_body,
  "Supplementary Section~S5 specializes the exchangeable-array bootstrap result to the normalized game-level CDF; Sections~S7--S8 report the dependence sensitivities and finite-schedule null simulation.",
  "Appendix~\\ref{sec:supp-inference} specializes the exchangeable-array bootstrap result to the normalized game-level CDF; Appendices~\\ref{sec:supp-results}--\\ref{sec:supp-validation} report the dependence sensitivities and finite-schedule null simulation."
)
arxiv_body <- replace_fixed(
  arxiv_body,
  "Supplementary Figure~S6.1",
  "Appendix Figure~\\ref{fig:supp-fixed-clock}"
)
arxiv_body <- replace_fixed(arxiv_body, "Supplementary Section~S7", "Appendix~\\ref{sec:supp-results}")
arxiv_body <- replace_fixed(arxiv_body, "Section~S6 gives", "Appendix~\\ref{sec:supp-data} gives")

arxiv_appendix <- appendix_body
arxiv_appendix <- replace_fixed(
  arxiv_appendix,
  "Supplementary Section~S3",
  "Appendix~\\ref{app:two-player-derivation}"
)
arxiv_appendix <- replace_fixed(
  arxiv_appendix,
  "Sections~S5--S7",
  "Appendices~\\ref{sec:supp-inference}--\\ref{sec:supp-results}"
)
arxiv_appendix <- replace_fixed(arxiv_appendix, "Section~S8", "Appendix~\\ref{sec:supp-validation}")
arxiv_appendix <- replace_fixed(arxiv_appendix, "Section~S7", "Appendix~\\ref{sec:supp-results}")

arxiv_main <- c(
  "\\documentclass[12pt,letterpaper]{article}",
  "\\usepackage[utf8]{inputenc}",
  "\\usepackage[T1]{fontenc}",
  "\\usepackage{amsmath,amsfonts,amssymb,amsthm}",
  "\\usepackage{graphicx,geometry,parskip,placeins,enumitem}",
  "\\usepackage{xcolor}",
  "\\usepackage[authoryear]{natbib}",
  "\\definecolor{linkcolor}{RGB}{0,90,160}",
  "\\usepackage[colorlinks=true,linkcolor=linkcolor,urlcolor=linkcolor,citecolor=linkcolor]{hyperref}",
  "\\geometry{letterpaper,margin=2cm}",
  "\\newcommand{\\E}{\\mathbb{E}}",
  "\\renewcommand{\\P}{\\mathbb{P}}",
  "\\newcommand{\\V}{\\operatorname{Var}}",
  macro_lines,
  "\\theoremstyle{plain}",
  "\\newtheorem{theorem}{Theorem}",
  "\\newtheorem{lemma}[theorem]{Lemma}",
  "\\newtheorem{proposition}[theorem]{Proposition}",
  "\\newtheorem{corollary}[theorem]{Corollary}",
  "\\theoremstyle{definition}",
  "\\newtheorem{definition}[theorem]{Definition}",
  "\\newtheorem{remark}[theorem]{Remark}",
  "\\hypersetup{pdftitle={The Blown Lead Paradox: A Pathwise Calibration Benchmark for Win Probability Forecasts},pdfauthor={Jonathan Pipping-Gamon and Abraham J. Wyner},pdfkeywords={Forecast calibration, martingales, pigeonhole bootstrap, sports analytics, win probability}}",
  "\\title{The Blown Lead Paradox\\\\[0.25em]\\large A Pathwise Calibration Benchmark for Win Probability Forecasts}",
  "\\author{Jonathan Pipping-Gam\\'on \\and Abraham J. Wyner}",
  "\\date{August 25, 2026}",
  "\\begin{document}",
  "\\maketitle",
  "\\begin{abstract}",
  abstract_lines,
  "\\end{abstract}",
  "{\\small\\noindent\\textbf{Keywords:} \\mbox{forecast calibration}; martingales; \\mbox{pigeonhole bootstrap}; \\mbox{sports analytics}; \\mbox{win probability}.\\par}",
  "",
  arxiv_body,
  "",
  "\\section*{Acknowledgments}",
  ack_lines,
  "",
  "\\clearpage",
  "\\appendix",
  "\\renewcommand{\\thesection}{S\\arabic{section}}",
  "\\renewcommand{\\thesubsection}{S\\arabic{section}.\\arabic{subsection}}",
  "\\counterwithin{theorem}{section}",
  "\\counterwithin{table}{section}",
  "\\counterwithin{figure}{section}",
  "\\renewcommand{\\thetable}{\\thesection.\\arabic{table}}",
  "\\renewcommand{\\thefigure}{\\thesection.\\arabic{figure}}",
  "\\setcounter{section}{0}",
  arxiv_appendix,
  "\\bibliographystyle{apalike}",
  "\\bibliography{references}",
  "\\end{document}"
)
write_tex(arxiv_main, file.path(arxiv_dir, "main.tex"))

expected_arxiv_source_files <- sort(c(
  "main.tex",
  "references.bib",
  figure_paths
))
actual_arxiv_source_files <- sort(list.files(
  arxiv_dir,
  recursive = TRUE,
  all.files = TRUE,
  no.. = TRUE,
  include.dirs = FALSE
))
if (!identical(actual_arxiv_source_files, expected_arxiv_source_files)) {
  stop(
    "Unexpected structured arXiv package contents. Expected: ",
    paste(expected_arxiv_source_files, collapse = ", "),
    "; found: ",
    paste(actual_arxiv_source_files, collapse = ", "),
    "."
  )
}
if (!all(vapply(figure_paths, function(path) {
  any(grepl(path, arxiv_main, fixed = TRUE))
}, logical(1)))) {
  stop("A structured figure path is missing from the arXiv source.")
}

flat_arxiv_main <- arxiv_main
for (path in figure_paths) {
  flat_arxiv_main <- replace_fixed(
    flat_arxiv_main,
    path,
    unname(arxiv_figure_names[[path]])
  )
}

expected_arxiv_zip_files <- sort(c(
  "main.tex",
  "references.bib",
  unname(arxiv_figure_names)
))

build_arxiv_zip <- function(zip_path) {
  stage_dir <- tempfile("arxiv-upload-")
  dir.create(stage_dir, recursive = TRUE)
  on.exit(unlink(stage_dir, recursive = TRUE), add = TRUE)

  write_tex(flat_arxiv_main, file.path(stage_dir, "main.tex"))
  copy_file(file.path(manuscript_dir, "references.bib"), file.path(stage_dir, "references.bib"))
  for (path in figure_paths) {
    copy_file(
      file.path(figure_root, path),
      file.path(stage_dir, unname(arxiv_figure_names[[path]]))
    )
  }

  old_wd <- getwd()
  on.exit(setwd(old_wd), add = TRUE)
  setwd(stage_dir)
  unlink(zip_path)
  status <- utils::zip(
    zipfile = zip_path,
    files = expected_arxiv_zip_files,
    flags = "-q -X"
  )
  if (!identical(as.integer(status), 0L)) {
    stop("Could not create the arXiv upload ZIP.")
  }
}

build_arxiv_zip(arxiv_zip)
zip_entries <- sort(utils::unzip(arxiv_zip, list = TRUE)$Name)
if (!identical(zip_entries, expected_arxiv_zip_files)) {
  stop("The arXiv upload ZIP does not contain the expected flat file set.")
}

writeLines(c(
  "# AOAS Submission Package",
  "",
  "Generated from `../manuscript/` by `../../scripts/manuscript/build-submission-packages.R`.",
  "The article uses the official IMS AOAS class and name-year bibliography style.",
  "Compile `article.tex`, `supplement.tex`, and `cover-letter.tex` separately with `latexmk -pdf`.",
  "The bibliographic title uses a colon; the working manuscript may retain its two-line title treatment."
), file.path(aoas_dir, "README.md"), useBytes = TRUE)

writeLines(c(
  "# arXiv Source Package",
  "",
  "`main.tex` is the only top-level document. Sections S1--S8 are appended to the article so all appendix references are internal hyperlinks.",
  "The source tree keeps figures organized under `figures/` and compiles with `latexmk -pdf main.tex`.",
  "For submission, upload `upload.zip`; it contains the same TeX, bibliography, and figures in a flat twelve-file archive."
), file.path(arxiv_dir, "README.md"), useBytes = TRUE)

message("Built self-contained AOAS and arXiv packages, including arxiv/upload.zip.")
