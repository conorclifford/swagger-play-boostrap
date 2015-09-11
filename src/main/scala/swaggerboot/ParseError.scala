package swaggerboot

case class ParseError(msg: String, replacement: String = "play.api.libs.json.JsValue", required: Boolean = false)
