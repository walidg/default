enum JSON:
    case Obj(bindings : Map[String, JSON])
    case Seq(elems : List[JSON])
    case Str(value : String)
    case Num(value : Double)
    case Bool(value : Boolean)
    case Null

def show(jsdata : JSON) : String = jsdata match
    case JSON.Seq(elems) => elems.map(show).mkString("[", ", ", "]")
    case JSON.Obj(bindings) => bindings.toList.map((b1, b2) => s"${inQuote(b1)} : ${show(b2)}").mkString("{", ",\n", "}")
    case JSON.Str(value) => s"${inQuote(value)}"
    case JSON.Num(value) => value.toString
    case JSON.Bool(value) => value.toString
    case JSON.Null => "null"

def inQuote(str : String) = "\"" + str + "\""

def query(data : JSON) : List[String] =
    def bindings(x: JSON): List[(String, JSON)] = x match
        case JSON.Obj(bindings) => bindings.toList
        case _ => Nil
    for
        case ("phoneNumbers", JSON.Seq(numberInfos)) <- bindings(jsData)
        numberInfo <- numberInfos
        case ("number", JSON.Str(number)) <- bindings(numberInfo)
        if number.startsWith("212")
    yield number

val jsData = JSON.Obj(Map(
    "firstName" -> JSON.Str("John"),
    "lastName" -> JSON.Str("Smith"),
    "address" -> JSON.Obj(Map(
    "streetAddress" -> JSON.Str("21 2nd Street"),
    "state" -> JSON.Str("NY"),
    "postalCode" -> JSON.Num(10021)
    )),
    "phoneNumbers" -> JSON.Seq(List(
    JSON.Obj(Map(
    "type" -> JSON.Str("home"), "number" -> JSON.Str("212 555-1234")
    )),
    JSON.Obj(Map(
    "type" -> JSON.Str("fax"), "number" -> JSON.Str("646 555-4567")
    )) )) ))
@main def test =
    println(show(jsData))
    println(query(jsData))

