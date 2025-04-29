package co.blocke.edi4j;

import java.util.*;
import co.blocke.edi4j.model4j.canonical.*;
import co.blocke.edi4j.model4j.refined.*;

import static java.util.stream.Collectors.toList;
import java.util.stream.Collectors;


public final class CanonicalParser {
    private CanonicalParser() {}

    // ==== convertCompositeField ====
    public static RefinedCompositeFieldSpec convertCompositeField(
            String name,
            String canonicalName,
            int index,
            EdiRefProperty prop,
            Map<String, EnumOrSchema> catalog
    ) throws CanonicalError {
        String canonicalNameWithIndex = canonicalName + String.format("%02d", index + 1);
        EdiSchema segment = prop.dereferenceSegment(catalog);
        List<RefinedSingleFieldSpec> fields = convertComponentFields(segment, canonicalNameWithIndex, catalog);
        return new RefinedCompositeFieldSpec(
                name,
                canonicalNameWithIndex,
                Optional.empty(),
                index + 1,
                "",
                segment.isRequired(name),
                fields
        );
    }

    // ==== convertComponentFields ====
    private static List<RefinedSingleFieldSpec> convertComponentFields(
            EdiSchema segment,
            String canonicalNameWithIndex,
            Map<String, EnumOrSchema> catalog
    ) throws CanonicalError {
        List<RefinedSingleFieldSpec> result = new ArrayList<>();
        int i = 0;
        for (Map.Entry<String, Property> entry : segment.getProperties().entrySet()) {
            String fname = entry.getKey();
            Property p = entry.getValue();
            if (p instanceof EdiElementProperty) {
                EdiElementProperty sp = (EdiElementProperty) p;
                Optional<Integer> elementId = sp.getxOpenediElementId().map(Integer::parseInt);
                String fieldCanon = canonicalNameWithIndex + String.format("%02d", i + 1);
                result.add(new RefinedSingleFieldSpec(
                        fname,
                        fieldCanon,
                        Optional.empty(),
                        i + 1,
                        "",
                        segment.isRequired(fname),
                        sp.getType(),
                        sp.getFormat(),
                        elementId,
                        Collections.emptyList(),
                        Optional.empty()
                ));
            } else {
                throw new CanonicalError("Unexpected property type " + fname);
            }
            i++;
        }
        return result;
    }

    // ==== convertFields ====
    private static List<RefinedFieldSpec> convertFields(
            EdiSchema segment,
            Map<String, EnumOrSchema> catalog
    ) throws CanonicalError {
        List<RefinedFieldSpec> result = new ArrayList<>();
        int i = 0;
        for (Map.Entry<String, Property> entry : segment.getProperties().entrySet()) {
            String fname = entry.getKey();
            Property p = entry.getValue();

            if (p instanceof EdiElementProperty) {
                EdiElementProperty sp = (EdiElementProperty) p;

                // figure out if there's a $ref in allOf
                Optional<String> enumValuesRef = sp.getAllOf()
                        .filter(l -> !l.isEmpty())
                        .flatMap(l -> Optional.ofNullable(l.get(0).get("$ref")));

                Optional<Integer> elementId = sp.getxOpenediElementId().map(Integer::parseInt);
                String baseCanon = segment.getId() + String.format("%02d", i + 1);

                if (enumValuesRef.isPresent()) {
                    String refKey = Canonical.extractRefKey(enumValuesRef.get());
                    result.add(new RefinedSingleFieldSpec(
                            fname,
                            baseCanon,
                            Optional.empty(),
                            i + 1,
                            "",
                            segment.isRequired(fname),
                            sp.getType(),
                            sp.getFormat(),
                            elementId,
                            sp.getEnums().orElse(Collections.emptyList()),
                            Optional.of(refKey)
                    ));
                }
                else {
                    result.add(new RefinedSingleFieldSpec(
                            fname,
                            baseCanon,
                            Optional.empty(),
                            i + 1,
                            "",
                            segment.isRequired(fname),
                            sp.getType(),
                            sp.getFormat(),
                            elementId,
                            sp.getEnums().orElse(Collections.emptyList()),
                            Optional.empty()
                    ));
                }

            }
            else if (p instanceof EdiRefProperty) {
                // composite field case
                result.add(convertCompositeField(
                        fname,
                        segment.getId(),
                        i,
                        (EdiRefProperty) p,
                        catalog
                ));

            }
            else {
                throw new CanonicalError(
                        "Unexpected property type " + fname + " with property " + p
                );
            }
            i++;
        }
        return result;
    }

    // ==== convertNestedLoopSegment ====
    private static RefinedLoopSpec convertNestedLoopSegment(
            String name,
            boolean isRequired,
            EdiSchema loopSchema,
            Map<String, EnumOrSchema> catalog
    ) throws CanonicalError {
        List<Map.Entry<String, Property>> entries = new ArrayList<>(loopSchema.getProperties().entrySet());
        if (entries.isEmpty()) {
            throw new CanonicalError("Malformed list spec for " + name);
        }
        Map.Entry<String, Property> head = entries.get(0);
        List<Map.Entry<String, Property>> tail = entries.subList(1, entries.size());

        EdiSchema loopSegmentSchema;
        if (head.getValue() instanceof EdiRefProperty) {
            loopSegmentSchema = ((EdiRefProperty) head.getValue()).dereferenceSegment(catalog);
        } else {
            throw new CanonicalError(
                    "Expected EdiRefProperty for first property of a loop with body (" + name + ")"
            );
        }

        List<RefinedFieldSpec> fields = convertFields(loopSegmentSchema, catalog);

        List<RefinedSingleOrLoopSegmentSpec> bodySegments = new ArrayList<>();
        for (Map.Entry<String, Property> e : tail) {
            String segName = e.getKey();
            Property prop2 = e.getValue();
            if (prop2 instanceof EdiRefProperty) {
                bodySegments.add(convertSegmentProperty(
                        segName,
                        loopSegmentSchema.isRequired(segName),
                        (EdiRefProperty) prop2,
                        catalog
                ));
            }
            else if (prop2 instanceof EdiItemsProperty) {
                EdiItemsProperty ip = (EdiItemsProperty) prop2;
                if (ip.loopHasBody(catalog)) {
                    bodySegments.add(convertLoopProperty(
                            segName,
                            loopSegmentSchema.isRequired(segName),
                            ip,
                            catalog
                    ));
                } else {
                    bodySegments.add(convertLoopPropertyNoBody(
                            segName,
                            loopSegmentSchema.isRequired(segName),
                            ip,
                            catalog
                    ));
                }
            }
            else {
                throw new CanonicalError(
                        "Element property not allowed in a loop body (" + name + ")"
                );
            }
        }

        String loopName = loopSegmentSchema.getId();
        return new RefinedLoopSpec(
                loopName,
                loopName,
                Optional.empty(),
                "",
                isRequired,
                loopSegmentSchema.getxOpenediSyntax().orElse(Collections.emptyList()),
                fields,
                Optional.of(1),
                Optional.of(1),
                bodySegments,
                Optional.empty()
        );
    }

    // ==== convertSegmentProperty ====
    public static RefinedSingleOrLoopSegmentSpec convertSegmentProperty(
            String name,
            boolean isRequired,
            EdiRefProperty prop,
            Map<String, EnumOrSchema> catalog
    ) throws CanonicalError {
        EdiSchema segment = prop.dereferenceSegment(catalog);
        if (segment.isLoop()) {
            return convertNestedLoopSegment(name, isRequired, segment, catalog);
        } else {
            return new RefinedSegmentSpec(
                    name,
                    name,
                    Optional.empty(),
                    "",
                    isRequired,
                    segment.getxOpenediSyntax().orElse(Collections.emptyList()),
                    convertFields(segment, catalog)
            );
        }
    }

    // ==== convertLoopProperty ====
    public static RefinedLoopSpec convertLoopProperty(
            String name,
            boolean isRequired,
            EdiItemsProperty prop,
            Map<String, EnumOrSchema> catalog
    ) throws CanonicalError {
        // first get the underlying schema
        EdiSchema loopSchema = prop.dereferenceSegment(catalog);

        // split first prop vs rest
        List<Map.Entry<String, Property>> entries = new ArrayList<>(loopSchema.getProperties().entrySet());
        if (entries.isEmpty()) {
            throw new CanonicalError("Malformed list spec for " + name);
        }
        Map.Entry<String, Property> head = entries.get(0);
        List<Map.Entry<String, Property>> tail = entries.subList(1, entries.size());

        EdiSchema loopSegmentSchema;
        if (head.getValue() instanceof EdiRefProperty) {
            loopSegmentSchema = ((EdiRefProperty) head.getValue()).dereferenceSegment(catalog);
        } else {
            throw new CanonicalError(
                    "Expected EdiRefProperty for first property of a loop with body (" + name + ")"
            );
        }

        List<RefinedFieldSpec> fields = convertFields(loopSegmentSchema, catalog);

        List<RefinedSingleOrLoopSegmentSpec> bodySegments = tail.stream().map(e -> {
            try {
                if (e.getValue() instanceof EdiRefProperty) {
                    return convertSegmentProperty(
                            e.getKey(),
                            loopSegmentSchema.isRequired(e.getKey()),
                            (EdiRefProperty) e.getValue(),
                            catalog
                    );
                } else if (e.getValue() instanceof EdiItemsProperty) {
                    EdiItemsProperty ip2 = (EdiItemsProperty) e.getValue();
                    if (ip2.loopHasBody(catalog)) {
                        return convertLoopProperty(
                                e.getKey(),
                                loopSegmentSchema.isRequired(e.getKey()),
                                ip2,
                                catalog
                        );
                    } else {
                        return convertLoopPropertyNoBody(
                                e.getKey(),
                                loopSegmentSchema.isRequired(e.getKey()),
                                ip2,
                                catalog
                        );
                    }
                } else {
                    throw new CanonicalError(
                            "Element property not allowed in a loop body (" + name + ")"
                    );
                }
            } catch (CanonicalError ce) {
                throw new RuntimeException(ce);
            }
        }).collect(toList());

        String loopName = loopSegmentSchema.getId();
        return new RefinedLoopSpec(
                loopName,
                loopName,
                Optional.empty(),
                "",
                isRequired,
                loopSegmentSchema.getxOpenediSyntax().orElse(Collections.emptyList()),
                fields,
                prop.getMinItems(),
                prop.getMaxItems(),
                bodySegments,
                Optional.empty()
        );
    }

    // ==== convertLoopPropertyNoBody ====
    public static RefinedLoopSpec convertLoopPropertyNoBody(
            String name,
            boolean isRequired,
            EdiItemsProperty prop,
            Map<String, EnumOrSchema> catalog
    ) throws CanonicalError {
        EdiSchema loopSchema = prop.dereferenceSegment(catalog);
        EdiSchema ediSchema = prop.dereferenceSegment(catalog);
        List<RefinedFieldSpec> fields = convertFields(ediSchema, catalog);
        return new RefinedLoopSpec(
                name,
                name,
                Optional.empty(),
                "",
                isRequired,
                loopSchema.getxOpenediSyntax().orElse(Collections.emptyList()),
                fields,
                prop.getMinItems(),
                prop.getMaxItems(),
                Collections.emptyList(),
                Optional.empty()
        );
    }

    // ==== toRefined ====
    public static RefinedDocumentSpec toRefined(
            EdiObject edi,
            String topLevel,
            String document,
            String version,
            String partner
    ) throws CanonicalError {
        Map<String, EnumOrSchema> catalog = edi.getComponents().getSchemas();
        EnumOrSchema top = catalog.get(topLevel);
        if (!(top instanceof EdiSchema)) {
            throw new CanonicalError(
                    "Can't find top level spec " + topLevel +
                            " in canonical spec, or it is the wrong type (EdiEnum)"
            );
        }
        EdiSchema topSchema = (EdiSchema) top;

        // filter only ref or items props
        List<Map.Entry<String, RefOrItemsProperty>> filtered =
                topSchema.getProperties()
                        .entrySet()
                        .stream()
                        // 1) keep only the entries whose value really is a RefOrItemsProperty
                        .filter(e -> e.getValue() instanceof RefOrItemsProperty)
                        // 2) map each one to a new Entry<String,RefOrItemsProperty>,
                        //    casting the value into the narrower type
                        .map(e -> new AbstractMap.SimpleEntry<>(
                                e.getKey(),
                                (RefOrItemsProperty)e.getValue()
                        ))
                        // 3) collect into a List
                        .collect(Collectors.toList());

        List<RefinedSingleOrLoopSegmentSpec> props = new ArrayList<>();
        for (Map.Entry<String, RefOrItemsProperty> e : filtered) {
            String name = e.getKey();
            if (e.getValue() instanceof EdiRefProperty) {
                props.add(convertSegmentProperty(
                        name,
                        topSchema.isRequired(name),
                        (EdiRefProperty) e.getValue(),
                        catalog
                ));
            } else {
                EdiItemsProperty ip3 = (EdiItemsProperty) e.getValue();
                if (ip3.loopHasBody(catalog)) {
                    props.add(convertLoopProperty(
                            name,
                            topSchema.isRequired(name),
                            ip3,
                            catalog
                    ));
                } else {
                    props.add(convertLoopPropertyNoBody(
                            name,
                            topSchema.isRequired(name),
                            ip3,
                            catalog
                    ));
                }
            }
        }

        return new RefinedDocumentSpec(document, version, partner, props);
    }
}
