package co.blocke.edi4j.serializers;

import com.fasterxml.jackson.core.*;
import com.fasterxml.jackson.databind.*;
import com.fasterxml.jackson.databind.node.ObjectNode;
import java.io.IOException;
import co.blocke.edi4j.model4j.canonical.*;

public class PropertyDeserializer extends JsonDeserializer<Property> {
    @Override
    public Property deserialize(JsonParser p, DeserializationContext ctxt)
            throws IOException, JsonProcessingException {
        ObjectCodec codec = p.getCodec();
        ObjectNode node  = codec.readTree(p);

        if (node.has("$ref")) {
            // "$ref" → EdiRefProperty
            return codec.treeToValue(node, EdiRefProperty.class);
        }
        else if (node.has("items")) {
            // "items" → EdiItemsProperty
            return codec.treeToValue(node, EdiItemsProperty.class);
        }
        else {
            // fallback → EdiElementProperty
            return codec.treeToValue(node, EdiElementProperty.class);
        }
    }
}

