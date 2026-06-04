//! RaTeX WASM bindings: parse LaTeX and return DisplayList as JSON for browser rendering.

use ratex_layout::{layout, to_display_list, LayoutOptions};
use ratex_parser::parse;
use serde_json::Value;
use wasm_bindgen::prelude::*;

/// Parse LaTeX string and return the display list as JSON.
/// The browser can deserialize this and draw with Canvas 2D (web-render).
///
/// # Errors
/// Returns a JS error string if parsing fails.
#[wasm_bindgen(js_name = "renderLatex")]
pub fn render_latex(latex: &str) -> Result<String, JsValue> {
    let nodes = parse(latex).map_err(|e| JsValue::from_str(&e.to_string()))?;
    let options = LayoutOptions::default();
    let layout_box = layout(&nodes, &options);
    let display_list = to_display_list(&layout_box);
    // Serialize via Value and sanitize: NaN/Infinity are invalid JSON, replace with 0
    let value = serde_json::to_value(&display_list).map_err(|e| JsValue::from_str(&e.to_string()))?;
    let sanitized = sanitize_json_numbers(value);
    serde_json::to_string(&sanitized).map_err(|e| JsValue::from_str(&e.to_string()))
}

/// Replace non-finite f64 and numeric nulls with 0 so JSON is valid in all runtimes.
fn sanitize_json_numbers(v: Value) -> Value {
    match v {
        Value::Number(n) => {
            if let Some(f) = n.as_f64() {
                if f.is_finite() {
                    Value::Number(n)
                } else {
                    Value::Number(serde_json::Number::from_f64(0.0).unwrap())
                }
            } else {
                Value::Number(n)
            }
        }
        Value::Null => Value::Null,
        Value::Array(arr) => Value::Array(arr.into_iter().map(sanitize_json_numbers).collect()),
        Value::Object(map) => Value::Object(
            map.into_iter()
                .map(|(k, v)| (k, sanitize_json_numbers(v)))
                .collect(),
        ),
        other => other,
    }
}
