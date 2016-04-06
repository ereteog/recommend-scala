import Math._
import scala.collection.mutable.Map

object Recommendation  {
  
	def euclideanDistance(point1: (Double, Double), point2:  (Double, Double)): Double = {
		sqrt(pow(point1._1-point2._1, 2)+pow(point1._2-point2._2, 2))
	}

	def euclideanSimilarity(point1: (Double, Double), point2:  (Double, Double)): Double = {
		1/(1+euclideanDistance(point1, point2))
	}
	
	def euclideanScore(prefs: Map[String, Map[String, Double]], person1: String, person2:String):Double ={
	  var sumsquares = 0.0
	  for(item <- prefs(person1).keys){
	    if(prefs(person2).contains(item)){
	    	sumsquares += pow(prefs(person1)(item) - prefs(person2)(item),2)
	    }
	  }
	  //livre propose: 1/(1+sumsquares), mais une distance euclidienne c'est la racine carrée de la somme des carrés.
	  //https://www.bionicspirit.com/blog/2012/01/16/cosine-similarity-euclidean-distance.html
	  if(sumsquares > 0){
		  1/(1+sqrt(sumsquares))
	  }
	  else {
		  0
	  }
	}
	
	def pearsonSimilarity(prefs: Map[String, Map[String, Double]], person1: String, person2:String):Double ={
	  var sumsquares1 = 0.0
	  var sumsquares2 = 0.0
	  var sum1 = 0.0
	  var sum2 = 0.0
	  var pSum = 0.0
	  var nbItem = 0.0
	  prefs(person1).keys.foreach(item => {
		  if(prefs(person2).contains(item)){
		    sum1 += prefs(person1)(item)
		    sum2 += prefs(person2)(item)
			sumsquares1 += pow(prefs(person1)(item),2)
			sumsquares2 += pow(prefs(person2)(item),2)
			pSum += prefs(person1)(item)*prefs(person2)(item)
			nbItem += 1
		  }
	  	}
	  )
	  if(nbItem > 0){
	    val num = pSum - (sum1*sum2)/nbItem
	    val den = sqrt((sumsquares1-pow(sum1,2)/nbItem)*(sumsquares2-pow(sum2,2)/nbItem))
	    if(den != 0) return num/den
	  }
	  return 0 
	}
	
	
	def topSimilarities(
	    prefs: Map[String, Map[String, Double]], 
	    nb: Int, 
	    sim_function: (Map[String, Map[String, Double]], String, String) => Double) : List[((String, String), Double)] ={
	  var similarityMap:Map[(String, String), Double] = Map()
	  for(i <- 0 to (prefs.size-1)){
	    val person1 = prefs.drop(i).head
		prefs.drop(i+1).foreach(person2 => {
			  similarityMap += (person1._1, person2._1) -> sim_function(prefs, person1._1, person2._1)
		})
	  }
	  return similarityMap.toList.sortBy(_._2).reverse.take(nb)
	}
	
	def topSimilarities(
	    prefs: Map[String, Map[String, Double]], 
	    person: String,
	    nb: Int, 
	    sim_function: (Map[String, Map[String, Double]], String, String) => Double) : List[(String, Double)] ={
	  var similarityMap:Map[String, Double] = Map()
	  prefs.foreach(other => {
		  if(person != other._1){
			  similarityMap += other._1 -> sim_function(prefs, person, other._1)		  
		  }
	  })
	  return similarityMap.toList.sortBy(_._2).reverse.take(nb)
	}
	
	def getRecommendations(
	    prefs: Map[String, Map[String, Double]], 
	    person: String,
	    nb: Int, 
	    sim_function: (Map[String, Map[String, Double]], String, String) => Double) : List[(String, Double)] ={
	  var totals:Map[String, Double] = Map().withDefault(x => 0)
	  var simSums:Map[String, Double] = Map().withDefault(x => 0)
	 // println(prefs)
	  prefs.foreach(other => {
		  if(person != other._1){
		    val similarity: Double = sim_function(prefs, person, other._1)
			if(similarity > 0){
				other._2.foreach(eval =>{
		        if(!prefs(person).contains(eval._1)){
		    	    totals += eval._1 -> (totals(eval._1) + eval._2 * similarity)
		    	    simSums += eval._1 -> (simSums(eval._1) + similarity)
		    	 }
		      })
		    }
		 
		  }
	  })
	  totals.foreach(total =>{
		  totals += total._1 -> (totals(total._1) / simSums(total._1))
	  })
	  return totals.toList.sortBy(_._2).reverse.take(nb)
	}
	
	def inversePrefs(prefs: Map[String, Map[String, Double]]): Map[String, Map[String, Double]] = {
	  var result: Map[String, Map[String, Double]] = Map()//.withDefault(x => Map())
	  prefs.foreach(entry => {
	    entry._2.foreach(eval => {
	      if(!result.contains(eval._1))
	    	 result += eval._1 -> Map()
	      result(eval._1) += (entry._1) -> eval._2
	    })
	})
	  
	  return result
	}
	
	
	def topGrowth(logs: Map[String, Map[String, Double]], nb: Int, point1:String, point2:String) : List[(String, Double)] ={
	  var resultMap:Map[String, Double] = Map()
	  logs.foreach{
	      case(akey:String, map:Map[String, Double]) 
			 => resultMap += akey -> (map(point2) - map(point1))/map(point1)}
	  return resultMap.toList.sortBy(_._2).reverse.take(nb)
	}
	
	def pente(point1: (Double, Double), point2:  (Double, Double)): Double = {
		(point1._2-point2._2)/(point1._1-point2._1)
	}

	
	def main(args:Array[String]){
		/*val critics = 
		    Map ("Lisa Rose" -> Map("Lady in the Water" ->2.5, "Snakes on a Plane" -> 3.5,
		    				"Just My Luck"-> 3.0, "Superman Returns"-> 3.5, 
		    				"You, Me and Dupree"-> 2.5, "The Night Listener"-> 1.0),
		    	"Gene Seymour" -> Map("Lady in the Water"-> 3.0, "Snakes on a Plane"-> 3.5,
							 "Just My Luck"-> 1.5, "Superman Returns"-> 5.0, "The Night Listener"-> 1.0,
							 "You, Me and Dupree"-> 3.5),
				"Michael Phillips"-> Map("Lady in the Water"-> 2.5, "Snakes on a Plane"-> 3.0,
							"Superman Returns"-> 3.5, "The Night Listener"-> 4.0),
				"Claudia Puig"-> Map("Snakes on a Plane"-> 3.5, "Just My Luck"-> 3.0,
							 "The Night Listener"-> 4.5, "Superman Returns"-> 4.0,
							 "You, Me and Dupree"-> 2.5),
				 "Mick LaSalle"-> Map("Lady in the Water"-> 3.0, "Snakes on a Plane"-> 4.0, 
				     "Superman Returns"-> 3.0, "The Night Listener"-> 3.0,
							 "You, Me and Dupree"-> 2.0),
				"Jack Matthews"-> Map("Lady in the Water"-> 3.0, "Snakes on a Plane"-> 4.0,
							"The Night Listener"-> 3.0, "Superman Returns"-> 5.0, "You, Me and Dupree"-> 3.5),
				"Toby"-> Map("Snakes on a Plane"->4.5,"You, Me and Dupree"->1.0,"Superman Returns"->4.0)
			)*/
		val critics = 
		    Map ("Lisa Rose" -> Map("Lady in the Water" ->1.0, "Snakes on a Plane" -> 1.0,
		    				"Just My Luck"-> 1.0, "Superman Returns"-> 1.0, 
		    				"You, Me and Dupree"->1.0, "The Night Listener"-> 1.0),
		    	"Gene Seymour" -> Map("Lady in the Water"->1.0, "Snakes on a Plane"->1.0,
							 "Just My Luck"->1.0, "Superman Returns"-> 1.0, "The Night Listener"-> 1.0,
							 "You, Me and Dupree"-> 1.0),
				"Michael Phillips"-> Map("Lady in the Water"->1.0, "Snakes on a Plane"-> 1.0,
							"Superman Returns"-> 1.0, "The Night Listener"-> 1.0),
				"Claudia Puig"-> Map("Snakes on a Plane"-> 1.0, "Just My Luck"->1.0,
							 "The Night Listener"-> 1.0, "Superman Returns"-> 1.0,
							 "You, Me and Dupree"-> 1.0),
				 "Mick LaSalle"-> Map("Lady in the Water"-> 1.0, "Snakes on a Plane"-> 1.0, 
				     "Superman Returns"-> 1.0, "The Night Listener"-> 1.0,
							 "You, Me and Dupree"-> 1.0),
				"Jack Matthews"-> Map("Lady in the Water"-> 1.0, "Snakes on a Plane"-> 1.0,
							"The Night Listener"-> 1.0, "Superman Returns"-> 1.0, "You, Me and Dupree"-> 1.0),
				"Toby"-> Map("Snakes on a Plane"->1.0,"You, Me and Dupree"->1.0,"Superman Returns"->1.0)
			)
		
		println("=============computing all similarities=================")
		var top5EuclideanSimilarity:List[((String, String), Double)] = topSimilarities(critics, 5, euclideanScore)
		var top5PearsonSimilarityMap:List[((String, String), Double)] = topSimilarities(critics, 5, pearsonSimilarity)
		
		println("----- top 5 euclidean similarties -----")
		top5EuclideanSimilarity.foreach {
		  case ((person1, person2), score) =>
			println("euclideanScore of "+ person1 + " AND "+person2 + ": " +score)
		}

		
		println("----- top 5 pearson similarties -----")
		top5PearsonSimilarityMap.foreach {
		  case ((person1, person2), score) =>
			println("pearsonSimilarity of "+ person1 + " AND "+person2 + ": " +score)
		}
		

		println("=============recommanding users=================")
		
		println("----- top 5 euclidean similarties of Toby -----")
		var top5ClaudiaEuclideanSimilarity:List[(String, Double)] = topSimilarities(critics, "Toby", 5, euclideanScore)
		top5ClaudiaEuclideanSimilarity.foreach {
		  case (person, score) =>
			println(person + ": " +score)
		}

		
		println("----- top 5 pearson similarties of Toby -----")
		var top5ClaudiaPearsonSimilarityMap:List[(String, Double)] = topSimilarities(critics, "Toby", 5, pearsonSimilarity)
		top5ClaudiaPearsonSimilarityMap.foreach {
		  case (person, score) =>
			println(person + ": " +score)
		}
		
		println("=============recommanding films=================")
		println("----- top euclidean recommendations for Toby -----")
		var top5ClaudiaEuclideanRecommendationMap:List[(String, Double)] = getRecommendations(critics, "Toby", 5, euclideanScore)
		top5ClaudiaEuclideanRecommendationMap.foreach {
		  case (film, score) =>
			println(film + ": " +score)
		}

		
		println("----- top pearson recommendations for Toby -----")
		var top5ClaudiaPearsonRecommendationMap:List[(String, Double)] = getRecommendations(critics, "Toby", 5, pearsonSimilarity)
		top5ClaudiaPearsonRecommendationMap.foreach {
		  case (film, score) =>
			println(film + ": " +score)
		}
		
		println("===== recommend similar films =====")
		println("----- inverse critics -----")
		val inverseCritics:Map[String, Map[String, Double]] = inversePrefs(critics)
		println(inverseCritics)
		
		
		println("----- top 5 euclidean similarties of Superman Returns -----")
		var top5SupermanReturnsEuclideanSimilarity:List[(String, Double)] = topSimilarities(inverseCritics, "Superman Returns", 5, euclideanScore)
		top5SupermanReturnsEuclideanSimilarity.foreach {
		  case (person, score) =>
			println(person + ": " +score)
		}

		
		println("----- top 5 pearson similarties of Superman Returns -----")
		var top5SupermanReturnsPearsonSimilarityMap:List[(String, Double)] = topSimilarities(inverseCritics, "Superman Returns", 5, pearsonSimilarity)
		top5SupermanReturnsPearsonSimilarityMap.foreach {
		  case (person, score) =>
			println(person + ": " +score)
		}

		
		println("=============recommanding a person to invite for Just My Luck =================")
		println("----- top euclidean recommendations for Just My Luck -----")
		var top5JustMyLuckEuclideanRecommendationMap:List[(String, Double)] = getRecommendations(inverseCritics, "Just My Luck", 5, euclideanScore)
		top5JustMyLuckEuclideanRecommendationMap.foreach {
		  case (film, score) =>
			println(film + ": " +score)
		}

		
		println("----- top pearson recommendations for Just My Luck -----")
		var top5JustMyLuckPearsonRecommendationMap:List[(String, Double)] = getRecommendations(inverseCritics, "Just My Luck", 5, pearsonSimilarity)
		top5JustMyLuckPearsonRecommendationMap.foreach {
		  case (film, score) =>
			println(film + ": " +score)
		}

		println("=============trying somethin with tags=================")
		
		var tags:Map[String,Map[String,Double]] = 
		    Map ("tag1" -> Map("h1" ->2.0, "h2" -> 3.0,
		    				"h3"-> 10.0, "h4"-> 20.0),
		    	"tag2" -> Map("h1"-> 10.0, "h2"-> 6.0,
							 "h3"-> 8.0, "h4"-> 50),
				 "tag3" -> Map("h1" ->5, "h2" -> 1,
		    				"h3"-> 12, "h4"-> 4),
				 "tag4" -> Map("h1" ->1, "h2" -> 1,
		    				"h3"-> 8, "h4"-> 20),
				 "tag5" -> Map("h1" ->1000, "h2" -> 5,
		    				"h3"-> 5, "h4"-> 1200))
		
		    				
		    				
		var top3Growth:List[(String, Double)] = topGrowth(tags, 3, "h1", "h4")
		var top3EuclideanSimilarity:List[((String, String), Double)] = topSimilarities(tags, 3, euclideanScore)
		var top3PearsonSimilarity:List[((String, String), Double)] = topSimilarities(tags, 3, pearsonSimilarity)
		
		println("-------- top 3 growth --------")
		top3Growth.foreach {
		  case (tag, score) =>
			println("growth of "+ tag + ": " +score)
		}
		
		println("----- top 5 euclidean similarties -----")
		top3EuclideanSimilarity.foreach {
		  case ((person1, person2), score) =>
			println("euclideanScore of "+ person1 + " AND "+person2 + ": " +score)
		}

		
		println("----- top 5 pearson similarties -----")
		top3PearsonSimilarity.foreach {
		  case ((person1, person2), score) =>
			println("pearsonSimilarity of "+ person1 + " AND "+person2 + ": " +score)
		}
				
/* item- based filtering usually outperforms user-based filtering in sparse datasets and the two perform about equally in dense datasets
 other index:
http://en.wikipedia.org/wiki/Jaccard_index

http://en.wikipedia.org/wiki/Collaborative_filtering
* */
	}
}