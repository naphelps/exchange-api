package org.openhorizon.exchangeapi.table.apikey

import slick.jdbc.PostgresProfile.api._
import slick.lifted.{TableQuery, Rep, Query}

object ApiKeysTQ extends TableQuery(new ApiKeys(_)) { 
  //we are using sha256->bcrypt now so this method is no longer working
  // def getUsernameByHashedKey(hash: String): Query[Rep[String], String, Seq] =
  //   this.filter(_.hashedKey === hash).map(_.username)
  
  def getByUser(username: String): Query[ApiKeys, ApiKeyRow, Seq] =
    this.filter(_.username === username)

  def getById(id: String): Query[ApiKeys, ApiKeyRow, Seq] =
    this.filter(_.id === id)

  def insert(apiKey: ApiKeyRow): DBIO[Int] = this += apiKey

  def getByOrg(orgid: String): Query[ApiKeys, ApiKeyRow, Seq] =
    this.filter(_.orgid === orgid)
}
