package jp.sf.amateras.solr.scala

import org.apache.solr.common._
import org.apache.solr.common.util._
import org.apache.solr.client.solrj._
import impl.HttpSolrServer
import org.apache.http.auth.UsernamePasswordCredentials
import org.apache.http.impl.client.AbstractHttpClient
import org.apache.http.{HttpRequest, HttpRequestInterceptor}
import org.apache.http.protocol.HttpContext
import org.apache.http.impl.auth.BasicScheme

object SolrServerFactory {

  /**
   * Configure SolrClient for basic authentication.
   *
   * {{{
   * implicit val server = SolrServerFactory.basicAuth("username", "password")
   * val client = new SolrClient("http://localhost:8983/solr")
   * }}}
   */
  def basicAuth(username: String, password: String) = (url: String) => {
    val server = new HttpSolrServer(url)
    val credentials = new UsernamePasswordCredentials(username, password)
    server.getHttpClient.asInstanceOf[AbstractHttpClient].addRequestInterceptor(new HttpRequestInterceptor {
      def process(request: HttpRequest, context: HttpContext) {
        request.addHeader(BasicScheme.authenticate(credentials, "US-ASCII", false))
      }
    })
    server
  }

  /**
   * Provides the dummy SolrServer for unit testing.
   *
   * {{{
   * implicit val server = SolrServerFactory.dummy { request =>
   *   println(request.getMethod)
   *   println(request.getPath)
   *   println(request.getParams)
   * }
   * val client = new SolrClient("http://localhost:8983/solr")
   * }}}
   */
  def dummy(listener: (SolrRequest) => Unit) = (url: String) => new SolrServer {
    def request(request: SolrRequest): NamedList[Object] = {
      listener(request)

      val response = new SimpleOrderedMap[Object]()
      response.add("response", new SolrDocumentList())

      response
    }

    def shutdown() {}
  }

}