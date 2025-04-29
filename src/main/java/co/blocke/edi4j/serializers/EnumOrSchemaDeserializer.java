package co.blocke.edi4j.serializers;

import java.io.IOException;

import com.fasterxml.jackson.core.*;
import com.fasterxml.jackson.databind.*;
import com.fasterxml.jackson.databind.node.ObjectNode;
import co.blocke.edi4j.model4j.canonical.*;

public class EnumOrSchemaDeserializer
        extends JsonDeserializer<EnumOrSchema> {

    @Override
    public EnumOrSchema deserialize(JsonParser p, DeserializationContext ctxt)
            throws IOException, JsonProcessingException {
        ObjectCodec codec = p.getCodec();
        ObjectNode node  = codec.readTree(p);

        // decide based on presence of the "enum" field
        if (node.has("enum")) {
            // it's an EdiEnum
            return codec.treeToValue(node, EdiEnum.class);
        }
        // otherwise assume it's an EdiSchema
        return codec.treeToValue(node, EdiSchema.class);
    }
}
