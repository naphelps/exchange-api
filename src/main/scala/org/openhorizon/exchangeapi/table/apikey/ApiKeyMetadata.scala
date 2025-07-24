package org.openhorizon.exchangeapi.table.apikey
import com.fasterxml.jackson.annotation.{JsonProperty,JsonInclude}
import java.time.ZoneId

@JsonInclude(JsonInclude.Include.NON_NULL)
final case class ApiKeyMetadata(description: String = "",
                                id: String,
                                label: String = "",
                                lastUpdated: String,
                                @JsonInclude(JsonInclude.Include.NON_NULL)
                                owner: String = null)
{
  def this(row: ApiKeyRow,
           ownerStr: String) =
    this(id = row.id.toString,
         description = row.description.getOrElse(""),
         label = row.label.getOrElse(""),
         lastUpdated = row.modifiedAt.toInstant.atZone(ZoneId.of("UTC")).withZoneSameInstant(ZoneId.of("UTC")).toString,
         owner = ownerStr)
}