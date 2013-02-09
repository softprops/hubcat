package object hubcat {
  import dispatch.FunctionHandler
  import com.ning.http.client.Response

  implicit def r2h[T](f: Response => T): Client.Handler[T] =
    new FunctionHandler(f)
}
