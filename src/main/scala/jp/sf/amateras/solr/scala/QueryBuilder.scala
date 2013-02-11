package jp.sf.amateras.solr.scala

import scala.collection.JavaConverters._

import org.apache.solr.client.solrj.SolrQuery
import org.apache.solr.client.solrj.SolrServer
import org.apache.solr.common.SolrDocumentList

import jp.sf.amateras.solr.scala.query.ExpressionParser
import jp.sf.amateras.solr.scala.query.QueryTemplate
import org.apache.solr.common.params.CommonParams

class QueryBuilder(server: SolrServer, query: String)(implicit parser: ExpressionParser) {

  private val solrQuery = new SolrQuery(query)

  /**
   * Sets field names to retrieve by this query.
   *
   * @param fields field names
   */
  def fields(fields: String*): QueryBuilder = {
    fields.foreach { field =>
      solrQuery.addField(field)
    }
    this
  }

  /**
   * Sets the sorting field name and its order.
   *
   * @param field the sorting field name
   * @param order the sorting order
   */
  def sortBy(field: String, order: Order): QueryBuilder = {
    solrQuery.addSortField(field, order)
    this
  }

  /**
   * Sets grouping field names.
   *
   * @param fields field names
   */
  def groupBy(fields: String*): QueryBuilder = {
    if(fields.size > 0){
      solrQuery.setParam("group", "true")
      solrQuery.setParam("group.field", fields: _*)
    }
    this
  }

  /**
   * Sets facet field names.
   *
   * @param fields field names
   */
  def facetFields(fields: String*): QueryBuilder = {
    solrQuery.setFacet(true)
    solrQuery.addFacetField(fields: _*)
    this
  }

  /**
   * Sets filter queries.
   *
   * @param filterQueries filter queries
   */
  def filter(filterQueries: String*) = {
    solrQuery.addFilterQuery(filterQueries: _*)
    this
  }

  /**
   * Specifies the maximum number of results to return.
   *
   * @param rows number of results
   */
  def rows(rows: Int) = {
    solrQuery.setRows(rows)
    this
  }

  /**
   * Sets the offset to start at in the result set.
   *
   * @param start zero-based offset
   */
  def start(start: Int) = {
    solrQuery.setStart(start)
    this
  }

  /**
   * Sets request handler.
   *
   * @param qt request handler
   */
  def requestHandler(qt: String) = {
    solrQuery.setRequestHandler(qt)
    this
  }

  /**
   * Sets query parameter.
   *
   * @param name parameter name
   * @param values parameter values to add
   */
  def param(name: String, values: String*) = {
    solrQuery.add(name, values: _*)
    this
  }

  /**
   * Calls arbitrary function over SolrJ query object.
   *
   * @param configurator configurator function
   */
  def configure(configurator: (SolrQuery => Unit)) = {
    configurator(solrQuery)
    this
  }

  /**
   * Returns the search result of this query as List[Map[String, Any]].
   *
   * @param params the parameter map or case class which would be given to the query
   * @return the search result
   */
  def getResultAsMap(params: Any = null): MapQueryResult = {

    def toList(docList: SolrDocumentList): List[Map[String, Any]] = docList match {
      case null => Nil
      case _ => (for(i <- 0 to docList.size() - 1) yield {
        val doc = docList.get(i)
        doc.getFieldNames.asScala.map { key => (key, doc.getFieldValue(key)) }.toMap
      }).toList
    }

    def mergeParams(name: String) = if (params != null) {
      val values = solrQuery.getParams(name)
      if (values != null) {
        values.transform(value => new QueryTemplate(value).merge(CaseClassMapper.toMap(params)))
      }
    }

    mergeParams(CommonParams.Q)
    mergeParams(CommonParams.FQ)

    val response = server.query(solrQuery)

    val numFound = if (response.getResults != null) response.getResults.getNumFound else 0
    val queryResult = solrQuery.getParams("group") match {
      case null => {
        toList(response.getResults)
      }
      case _ => {
        val groupResponse = response.getGroupResponse
        groupResponse.getValues.asScala.map { groupCommand =>
          groupCommand.getValues.asScala.map { group =>
            toList(group.getResult)
          }.flatten
        }.flatten.toList
      }
    }

    val facetResult = response.getFacetFields match {
      case null => Map.empty[String, Map[String, Long]]
      case facetFields => facetFields.asScala.map { field => (
          field.getName,
          field.getValues.asScala.map { value => (value.getName, value.getCount) }.toMap
      )}.toMap
    }

    val spellcheck = response.getSpellCheckResponse match {
      case null => new SpellcheckResult(Nil, Map.empty)
      case spell => {
        val collations = if (spell.getCollatedResults != null) spell.getCollatedResults.asScala else Nil
        val suggestions = spell.getSuggestionMap.asScala
        new SpellcheckResult(
          collations.map(_.getCollationQueryString).toList,
          suggestions.mapValues(_.getAlternatives.asScala.toList).toMap
        )
      }
    }

    MapQueryResult(numFound, queryResult, facetResult, spellcheck)
  }

  /**
   * Returns the search result of this query as the case class.
   *
   * @param params the parameter map or case class which would be given to the query
   * @return the search result
   */
  def getResultAs[T](params: Any = null)(implicit m: Manifest[T]): CaseClassQueryResult[T] = {
    val result = getResultAsMap(params)

    CaseClassQueryResult[T](
      result.numFound,
      result.documents.map { doc =>
        CaseClassMapper.map2class[T](doc)
      },
      result.facetFields,
      result.spellcheck
    )

  }

}

