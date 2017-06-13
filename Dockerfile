FROM flangelier/scala

RUN wget https://github.com/rasantos24/LP_Scala/archive/master.zip
RUN unzip master.zip

EXPOSE 8080

CMD cd LP_Scala-master && scalac server.scala && scala Server
