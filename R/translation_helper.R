Sys.setenv(DEEPL_API_KEY = keyring::key_get("deepl"))

babeldown::deepl_translate(
  "quarto-training.fr.qmd",
  out_path = "quarto-training.en.qmd",
  source_lang = "FR",
  target_lang = "EN-US"
)
