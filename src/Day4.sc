import java.security.MessageDigest

Iterator.from(0).find(getHex(_).startsWith("000000"))

def getHex(n: Int) = MessageDigest
  .getInstance("MD5")
  .digest(("bgvyzdsv" + n).getBytes)
  .map("%02X".format(_))
  .mkString