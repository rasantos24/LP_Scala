import java.net.HttpURLConnection;
import java.nio.ByteBuffer;
import java.io.IOException;
import java.io.OutputStream
import java.net.InetSocketAddress
import java.net.URL
import java.io.BufferedReader
import java.io.InputStreamReader
import java.util.Base64
import javax.imageio.ImageIO
import java.io.ByteArrayInputStream
import java.io.ByteArrayOutputStream
import java.awt.image.BufferedImage
import java.nio.charset.Charset

import com.sun.net.httpserver.HttpExchange
import com.sun.net.httpserver.HttpHandler
import com.sun.net.httpserver.HttpServer

import com.google.gson._;

import scala.collection.mutable.ArrayBuffer;
import scala.collection.JavaConversions._;
import scala.collection.JavaConverters._;

import java.awt.image.DataBufferByte
import java.lang.Object
import java.awt.image.Raster
import java.awt.Color
import scala.util.parsing.json.JSON
import util.control.Breaks._
import java.io.FileInputStream
import java.io.FileOutputStream
import java.io.File

object Main {
  def main(args: Array[String]): Unit = {

    val server = HttpServer.create(new InetSocketAddress(8080), 0)
    server.createContext("/tarea1", new tarea1())
    server.createContext("/tarea2", new tarea2())
    server.createContext("/tarea3", new tarea3())
    server.createContext("/tarea4", new tarea4())
    server.setExecutor(null)
    server.start()
  }
}

class tarea1() extends HttpHandler{
     override def handle(t: HttpExchange){
        if (t.getRequestMethod() == "POST") {
          val os: OutputStream = t.getResponseBody();
          try {
            val respo = scala.io.Source.fromInputStream(t.getRequestBody()).mkString
            var myInput: JsonObject = new JsonParser().parse(new String(respo)).getAsJsonObject();
            val origen: String = myInput.get("origen").getAsString().replace(" ", "+");
            val destino: String = myInput.get("destino").getAsString().replace(" ", "+");
            println("Origen: " + origen);
            println("Destino: " + destino);
            var url: String = "https://maps.googleapis.com/maps/api/directions/json?origin="+origen+"&destination="+destino+"&key=AIzaSyDm3w4oPw82M8upaATU0IY5Sm_-tNvaXp8";
            var getRespo: String = sendGet(url);
            val jsArr = new JsonParser().parse(getRespo).getAsJsonObject().get("routes").getAsJsonArray().get(0).getAsJsonObject().get("legs").getAsJsonArray().get(0).getAsJsonObject().get("steps").getAsJsonArray();
            var mapRespo:scala.collection.mutable.Map[String, ArrayBuffer[JsonObject]] = scala.collection.mutable.Map[String, ArrayBuffer[JsonObject]]();
            mapRespo("ruta") = new ArrayBuffer[JsonObject]();
            mapRespo("ruta") += (jsArr.get(0).getAsJsonObject().get("start_location").getAsJsonObject());
            for (dir <- jsArr)
             mapRespo("ruta") += (dir.getAsJsonObject().get("end_location").getAsJsonObject());

            var gson = new GsonBuilder().create();
            var json = gson.toJson(mapRespo.asJava.get("ruta").asJava);

            json = "{\"ruta\":" + json + "}";
            json = json.replace("ng", "on");
            t.getResponseHeaders().add("content-type", "json");
            t.sendResponseHeaders(200, json.getBytes("UTF-8").size.asInstanceOf[Number].longValue );
            os.write(json.getBytes("UTF-8"));
            os.close();
          }
          catch {
            case e: Exception =>
              e.printStackTrace();
              t.getResponseHeaders().add("content-type", "json");
              var x = "{\"error\":\"algo salio mal\"}";
              t.sendResponseHeaders(500, x.getBytes("UTF-8").size.asInstanceOf[Number].longValue);
              os.write(x.getBytes("UTF-8"));
              os.close();
          }
        }
    }

    def sendGet(url: String): String = {

      val obj: URL = new URL(url);
      val con: HttpURLConnection = obj.openConnection().asInstanceOf[HttpURLConnection];

      con.setRequestMethod("GET");

      val responseCode: Int = con.getResponseCode();
      if (responseCode != 200)
        return "nil"

      val str = new String(scala.io.Source.fromInputStream(con.getInputStream()).mkString);
      return str;
    }
}

class tarea2() extends HttpHandler{
  override def handle(t: HttpExchange){
      if (t.getRequestMethod() == "POST") {
          val os: OutputStream = t.getResponseBody()
          var input = t.getRequestBody()
          var respo: Array[Byte] = Stream.continually(input.read).takeWhile(_ != -1).map(_.toByte).toArray
          val myString = new String(respo, Charset.forName("UTF-8"))
          val parts = myString.split("\"")
          val origen = parts(3)
          
          val getDireccion = "https://maps.googleapis.com/maps/api/directions/json?origin="+origen+"&destination="+origen+"&key=AIzaSyDxjE_B2p2t199wTZPwBErRiOmaqL1hkhM"
          val url = new URL(getDireccion.replace(' ', '+'))
          val buff = new BufferedReader(new InputStreamReader(url.openStream()))
          var maps: String = ""
          var temp: String = ""
          while(buff.ready()){
              temp = buff.readLine()
              maps = maps + temp
          }
          val legs = maps.split("start_location")
          val tempo = legs(1).replace(" ","")
          var i = 9
          var valor : String = ""
          while(i < tempo.length){
              valor = valor + tempo(i)
              if(tempo(i).equals('}')){
                  println("BREAK")
                  i = tempo.length
              }
              i+= 1
          }
          val get_values = valor.split(",")
          var lat = get_values(0)
          val lng_c = get_values(1).replace("\"lng\":","")
          val lng = lng_c.replace("}","")
          val newOrigen = lat +","+lng
          println(newOrigen)
          val get_places = "https://maps.googleapis.com/maps/api/place/nearbysearch/json?location="+newOrigen+"&radius=300&types=restaurant&key=AIzaSyDxjE_B2p2t199wTZPwBErRiOmaqL1hkhM"
          val url_p = new URL(get_places.replace(' ', '+'))
          val brf = new BufferedReader(new InputStreamReader(url_p.openStream()))
          var maps2: String = ""
          var temp2: String = ""
          while(brf.ready()){
              temp2 = brf.readLine()
              maps2 = maps2 + temp2
          }
          val location_map = maps2.split("location") 
          val name_map = maps2.split("name")
          println(location_map(1))
          
          println(name_map(1))
          
          var count = 0
          var result1 : String = ""
          var result2 : String = ""
          var x = 0
          var location_tmp = new Array[String](location_map.length-1)
          while(x < (location_map.length -1)){
              location_tmp(x) = location_map(x+1)
              x+= 1
          }
          for (i <- location_tmp ){
              val tempo = i
              val newTemp = tempo.replace(" ", "")
              var j = 3
              while(j < newTemp.length){
                  result1 = result1 + newTemp(j)
                  if(newTemp(j).equals('}')){
                      println("AGMH")
                      if(count < (location_tmp.length-1)){
                          result1 = result1 + ','
                      }
                      j = newTemp.length
                      count += 1
                  }
                  j += 1
              }
              j = 3
          }
          
          var name_tmp = new Array[String](name_map.length-1)
          var y = 0
          while(y < (name_map.length -1)){
              name_tmp(y) = name_map(y+1)
              y+= 1
          }
          for(i <- name_tmp){
              val tmp = i
              var j = 4
              while(j < tmp.length){
                  result2 = result2 + tmp(j)
                  if(tmp(j).equals(',')){
                      println("BREAK")
                      if(count < (name_tmp.length-1)){
                          result2 = result2 + ','
                      }
                      j = tmp.length
                      count += 1
                  }
                  j += 1
              }
              j = 4
          }
          val names = result2.split(",")
          val locations = result1.split(",")

          println("names: ")
          for(i <- names){
              println(i)
          }
          println("locations: ")
          for(i <- locations){
              println(i)
          }
          var a = 0
          var b = 0
          var json_result : String = "{\"Restaurantes\":["
          while(a < names.length){
              json_result = json_result + "{\"Nombre\":" + names(a) + ","  
              json_result = json_result + locations(b) + "," + locations(b+1)
              if(a == (names.length-1)){
                  a = names.length
              }else{
              json_result = json_result + ","
              b+=2
              a+=1
              }
          }
          var json_places = json_result + "]}"
          println(json_places)
          respo = json_places.getBytes(Charset.forName("UTF-8"))
          t.getResponseHeaders().add("content-type", "json")
          t.sendResponseHeaders(200, respo.size.toLong)
          os.write(respo)
          os.close()
      }
  }
}

class tarea3() extends HttpHandler{
  override def handle(t: HttpExchange){
    if (t.getRequestMethod == "POST") {

      val os: OutputStream = t.getResponseBody

      val input = t.getRequestBody
      var response: Array[Byte] = Stream.continually(input.read).takeWhile(_ != -1).map(_.toByte).toArray
      val test = new String(response, Charset.forName("UTF-8"))
      val idk = test.split("\"")
      val nombre = idk(3)
      val img_data = idk(7)
      var gray_img = ""
      val img = Base64.getDecoder.decode(img_data)
      val bais: ByteArrayInputStream = new ByteArrayInputStream(img)
      val editable_img: BufferedImage = ImageIO.read(bais)

      for(x <- 0 until editable_img.getWidth()){
        for(y <- 0 until editable_img.getHeight()){
          val rgb = editable_img.getRGB(x, y)
          val r = (rgb >> 16) & 0xFF
          val g = (rgb >> 8) & 0xFF
          val b = rgb & 0xFF

          val grayLevel = (0.21 * r + 0.72 * g + 0.07 * b).toInt
          val gray = grayLevel << 16 | (grayLevel << 8) | grayLevel

          editable_img.setRGB(x, y, gray)
        }
      }


      val baos: ByteArrayOutputStream = new ByteArrayOutputStream()
      ImageIO.write(editable_img, "bmp", baos)
      val new_img = baos.toByteArray
      gray_img = Base64.getEncoder.encodeToString(new_img)

      var json = ""
      val name = nombre.split("\\.")
      json = "{\"nombre\":\"" + name(0) + "." + name(1) + "\", \"data\": \"" + gray_img + "\"}"

      response = json.getBytes(Charset.forName("UTF-8"))
      t.getResponseHeaders.add("content-type", "json")
      t.sendResponseHeaders(200, response.length.toLong)
      os.write(response)
      os.close()
    }
  }
}

class tarea4() extends HttpHandler{
  override def handle(t: HttpExchange){
      if (t.getRequestMethod() == "POST") {
          
          val os: OutputStream = t.getResponseBody()
          var input = t.getRequestBody()
          var respo: Array[Byte] = Stream.continually(input.read).takeWhile(_ != -1).map(_.toByte).toArray
          val myString = new String(respo, Charset.forName("UTF-8"))
          val parts = myString.split("\"")
          
          val nombre = parts(3)
          val data = parts(7)
          val anch = parts(12).split(":")
          val alto = parts(14).split(":")
          val anch2 = anch(1).split(",")
          val alto2 = alto(1).split("}")
          println(anch2(0))
          println(alto2(0))

          var imgAncho = anch2(0).toInt
          var imgAlto = alto2(0).toInt
          println(imgAncho)
          println(imgAlto)

          var img = Base64.getDecoder().decode(data)
          var bais: ByteArrayInputStream = new ByteArrayInputStream(img)
          var ediIMG: BufferedImage = ImageIO.read(bais)
          var smallIMG: BufferedImage = new BufferedImage(imgAncho, imgAlto, 1)
          var altura = ediIMG.getHeight()
          var ancho = ediIMG.getWidth()
          var divX = ancho.toFloat/imgAncho.toFloat
          var divY = altura.toFloat/imgAlto.toFloat
          println(divX)
          println(divY)
          
          var resAncho = (ancho/divX).toInt
          var resAltura = (altura/divY).toInt
          println("" + ancho + " / " + imgAncho + " = " + divX)
          println("" + altura + " / " + imgAlto + " = " + divY)

          for(x <- 0 to resAncho - 1){
              for(y <- 0 to resAltura - 1){
                  var pixel = ediIMG.getRGB((x * divX).toInt, (y * divY).toInt)
                  smallIMG.setRGB(x, y, pixel)
              }
          }
          var baos: ByteArrayOutputStream = new ByteArrayOutputStream()
          ImageIO.write(smallIMG, "bmp", baos)
          var newIMG = baos.toByteArray()
          val smallIMG2 = Base64.getEncoder().encodeToString(newIMG)

          var json= ""
          println(nombre)
          var newNombre = nombre.split("\\.")
          json = "{\"nombre\":\"" + newNombre(0) + "(reducida)." + newNombre(1) + "\", \"data\": \"" + smallIMG2 + "\"}"
          println(json)
          respo = json.getBytes(Charset.forName("UTF-8"))
          
          t.getResponseHeaders().add("content-type", "json")
          t.sendResponseHeaders(200, respo.size.toLong)
          os.write(respo)
          os.close()
      }
  }
}