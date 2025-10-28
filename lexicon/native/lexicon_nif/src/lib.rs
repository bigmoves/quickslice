use serde_json::Value;

// NIF function to validate multiple lexicon schema documents
#[rustler::nif]
fn validate_schemas(json_strings: Vec<String>) -> Result<String, String> {
    // Parse JSON strings into serde_json Values
    let mut lexicons: Vec<Value> = Vec::new();

    for json_str in json_strings {
        let lexicon: Value =
            serde_json::from_str(&json_str).map_err(|e| format!("Failed to parse JSON: {}", e))?;
        lexicons.push(lexicon);
    }

    // Validate the lexicons using slices-lexicon
    match slices_lexicon::validate(lexicons) {
        Ok(_) => Ok("ok".to_string()),
        Err(errors) => {
            // Convert the error HashMap to a JSON string for easier handling in Gleam
            let error_json = serde_json::to_string(&errors)
                .unwrap_or_else(|_| "Failed to serialize validation errors".to_string());
            Err(error_json)
        }
    }
}

// NIF function to validate a record against lexicon schemas
#[rustler::nif]
fn validate_record(
    lexicon_jsons: Vec<String>,
    collection: String,
    record_json: String,
) -> Result<String, String> {
    // Parse lexicon JSON strings into serde_json Values
    let mut lexicons: Vec<Value> = Vec::new();
    for json_str in lexicon_jsons {
        let lexicon: Value = serde_json::from_str(&json_str)
            .map_err(|e| format!("Failed to parse lexicon JSON: {}", e))?;
        lexicons.push(lexicon);
    }

    // Parse the record JSON
    let record: Value = serde_json::from_str(&record_json)
        .map_err(|e| format!("Failed to parse record JSON: {}", e))?;

    // Validate the record against the lexicon schemas
    match slices_lexicon::validate_record(lexicons, &collection, record) {
        Ok(_) => Ok("ok".to_string()),
        Err(error) => Err(format!("{:?}", error)),
    }
}

// NIF function to check if a string is a valid NSID
#[rustler::nif]
fn is_valid_nsid(nsid: String) -> bool {
    slices_lexicon::is_valid_nsid(&nsid)
}

// Initialize the NIF module
rustler::init!("lexicon_nif");
