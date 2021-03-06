package jp.sf.amateras.solr.scala

/**
 * The result of query which is executed by QueryBuilder#getResultAsMap().
 *
 * @param numFound the total number of hits
 * @param documents the list of documents which are matched to the query
 * @param facetFields the facet count of fields which were specified by QueryBuilder#facetFields()
 */
case class MapQueryResult(
    numFound: Long,
    documents: List[Map[String, Any]],
    facetFields: Map[String, Map[String, Long]],
    facetRanges: Map[String, Map[String, Int]],
    spellcheck: SpellcheckResult)

case class CaseClassQueryResult[T](
    numFound: Long,
    documents: List[T],
    facetFields: Map[String, Map[String, Long]],
    facetRanges: Map[String, Map[String, Int]],
    spellcheck: SpellcheckResult)

case class SpellcheckResult(
    collations: List[String],
    suggestions: Map[String, List[String]]) {
  def collation: Option[String] = collations.headOption
}
