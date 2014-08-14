package hubcat

import org.json4s.{ JBool, JString, JNothing, JValue, JInt }

/*trait Jsonizing {  
  def jStringOrNone(opt: Option[String]) =
    opt match { case Some(s) => JString(s) case _ => JNothing }

  def jIntOrNone(opt: Option[Int]) =
    opt match { case Some(i) => JInt(BigInt(i)) case _ => JNothing }

  def jBoolOrNone(opt: Option[Boolean]) =
    opt match { case Some(b) => JBool(b) case _ => JNothing }
}*/
