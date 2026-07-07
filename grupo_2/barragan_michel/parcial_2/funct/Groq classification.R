library(httr2)

# =============================================================================
# classify_abstract()
# Clasifica un abstract usando la API de Groq (llama-3.2-3b).
# Retorna un string con la categoría asignada:
#   Machine Learning, Generative AI, Statistics u Other.
# =============================================================================
classify_abstract <- function(abstract, api_key = 'XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX'){
  
  # Retornar NA si el abstract no existe
  if (is.na(abstract) || abstract == "") return(NA)
  
  tryCatch({
    
    response <- request("https://api.groq.com/openai/v1/chat/completions") |>
      req_auth_bearer_token(api_key) |>
      req_body_json(list(
        model = "llama-3.1-8b-instant",
        messages = list(
          list(role = "system", content = "Classify the following abstract into exactly one of these categories:
- Machine Learning: papers related to ML methodology, including clustering, classification, prediction, neural networks, deep learning, supervised/unsupervised learning, software implementations of ML algorithms, etc.
- Generative AI: papers related to generative models, including large language models, text/image generation, diffusion models, GANs, transformers, LLMs, etc.
- Statistics: papers related to statistical methodology, including inference, hypothesis testing, probability, regression, experimental design, statistical models, clinical trial design, and software implementations of statistical methods.
- Other: papers that apply existing methods to a specific domain problem without any methodological focus.

Reply in this exact format:
  Reasoning: <Reasoning behind why the category election>
  Classification: <category>"),
          list(role = "user", content = abstract)
        ),
        temperature = 0,
        seed        = 513
      )) |>
      req_timeout(30) |>
      req_perform() |>
      resp_body_json()
    
    response$choices[[1]]$message$content |>
      stringr::str_extract('(?<=Classification: ).*')
    
  }, error = \(e){
    if (grepl("429", conditionMessage(e))){
      message('\nRate limit alcanzado, esperando 60s...')
      Sys.sleep(60)
      classify_abstract(abstract, api_key)  # reintento recursivo
    } else {
      message(sprintf('\nError clasificando abstract: %s', conditionMessage(e)))
      NA
    }
  })
}
