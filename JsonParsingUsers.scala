package com.knoldus

import net.liftweb.json.DefaultFormats
import net.liftweb.json.JsonAST.JNothing
import net.liftweb.json.JsonAST.JValue
import org.apache.commons.io.IOUtils
import org.apache.http.client.methods.HttpGet
import org.apache.http.impl.client.HttpClientBuilder

import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.{Failure, Success}
import scala.concurrent.{Await, Future}
import scala.concurrent.duration._
import scala.language.implicitConversions


class JsonParsingUsers {

}
//case class User(id : String,name : String,username : String,email : String,address : String,geo : String,phone : String,website: String,company : String)
case class Geo(lat : String,lng : String)
case class Address(street : String,suite : String,city : String,zipcode : String,geo : Geo)
case class Company(name : String,catchPhrase : String,bs : String)

case class User(id : String,name : String,username : String,email : String,address: Address,phone : String,website: String,company: Company)

object JsonDataParsing{

  val url="https://jsonplaceholder.typicode.com/users"

  def readData : String = {
    val request = new HttpGet(url)
    val client = HttpClientBuilder.create().build()
    val response = client.execute(request)
    //println(response.getEntity.getContent.toString)
    val res=IOUtils.toString(response.getEntity.getContent)
    //println(res)
    res
  }
  def parse(jsonData : String) : List[User] ={

    implicit val formats: DefaultFormats.type = DefaultFormats

    val parsedJsonData = net.liftweb.json.parse(jsonData)

    //println(parsedJsonData)

    parsedJsonData.children map { user =>

     val id = (user \ "id").extract[String]
      //println(id)
     val name = (user \ "name").extract[String]
    //  println(id+"\t-\t"+name)
     val username = (user \ "username").extract[String]
     val email = (user \ "email").extract[String]
      val street = (user \ "address" \ "street").extract[String]
      val suite = (user \ "address" \ "suite").extract[String]
      val city = (user \ "address" \ "city").extract[String]
      val zipcode = (user \ "address" \ "zipcode").extract[String]
      val lat = (user \ "address" \ "geo" \ "lat").extract[String]
      val lng = (user \ "address" \ "geo" \ "lng").extract[String]

     val phone = (user \ "phone").extract[String]
     val website= (user \ "website").extract[String]
     val companyName = (user \ "company" \ "name").extract[String]
      val catchPhrase = (user \ "company" \ "catchPhrase").extract[String]
      val bs = (user \ "company" \ "bs").extract[String]
     // println(companyName)
      User(id,name,username,email,Address(street,suite,city,zipcode,Geo(lat,lng)),phone,website,Company(companyName,catchPhrase,bs))

    }
  }
  implicit val formats: DefaultFormats.type = DefaultFormats
  implicit def extract(json: JValue): String = json match {
    case JNothing => ""
    case data  => data.extract[String]
  }
}
object JsonParsingOb extends App{



  //implicit val formats = DefaultFormats
  //val res1= Await.result(jsonData,10.seconds)

  //val parsedJsonData =JsonDataParsing.parse(jsonData)


  //JsonDataParsing.parse(res1)

  val jsonData : Future[String] = Future{
    JsonDataParsing.readData
  }
  val parsedJsonData = for{
    jsonUserData<-jsonData
    parsedJsonData<- Future(JsonDataParsing.parse(jsonUserData))
  } yield parsedJsonData

  parsedJsonData onComplete{
    case Success(user) => println("User = "+user)
    case Failure(exception) => println(exception.getMessage)
  }

  val userData = Await.result(parsedJsonData, 10.seconds)
  println(userData)

  //println(parsedJsonData)
}
