package hubcat

import net.liftweb.json.{ JString, JNothing, JValue, JInt }

trait Jsonizing {  
  def jStringOrNone(opt: Option[String]) = opt match { case Some(s) => JString(s) case _ => JNothing }
  def jIntOrNone(opt: Option[Int]) = opt match { case Some(i) => JInt(BigInt(i)) case _ => JNothing }
}
