package co.blocke.edi4j;

import co.blocke.edi4j.model4j.Path;
import co.blocke.edi4j.model4j.difference.*;
import co.blocke.edi4j.model4j.refined.*;
import java.util.*;
import java.util.AbstractMap.SimpleEntry;
import java.util.stream.Collectors;

/**
 * Engine to compare three RefinedDocumentSpec instances (src, edi standard, target)
 * and produce a list of Difference objects describing mismatches.
 */
public final class DiffEngine {
    private DiffEngine() {}

    /** Top-level entry */
    public static List<Difference> compareSpecs(
            RefinedDocumentSpec src,
            RefinedDocumentSpec edi,
            RefinedDocumentSpec target
    ) {
        List<SegmentDifference> segDiffs = compareSegmentLists(
                new Path(),
                src.getSegments(),
                edi.getSegments(),
                target.getSegments(),
                new ArrayList<>()
        );
        return new ArrayList<>(segDiffs);
    }

    /** Segment list comparison */
    private static List<SegmentDifference> compareSegmentLists(
            Path path,
            List<RefinedSingleOrLoopSegmentSpec> src,
            List<RefinedSingleOrLoopSegmentSpec> edi,
            List<RefinedSingleOrLoopSegmentSpec> target,
            List<SegmentDifference> acc
    ) {
        if (src.isEmpty() && edi.isEmpty() && target.isEmpty()) return acc;
        if (edi.isEmpty()) {
            acc.add(new DifferenceError(path,
                    "Exhausted EDI standard segments before either src/target--extra elements present"
            ));
            return acc;
        }
        if (!src.isEmpty() && !edi.isEmpty() && !target.isEmpty()) {
            RefinedSingleOrLoopSegmentSpec s = src.get(0), e = edi.get(0), t = target.get(0);
            String sn = rawCName(s), en = rawCName(e), tn = rawCName(t);
            if (sn.equals(en) && en.equals(tn)) {
                acc.add(
                        s instanceof RefinedLoopSpec
                                ? compareTwoLoops(path, (RefinedLoopSpec)s, (RefinedLoopSpec)e, (RefinedLoopSpec)t)
                                : compareTwoSegments(path, (RefinedSegmentSpec)s, (RefinedSegmentSpec)e, (RefinedSegmentSpec)t)
                );
                return compareSegmentLists(path,
                        src.subList(1,src.size()), edi.subList(1,edi.size()), target.subList(1,target.size()), acc);
            } else if (sn.equals(en)) {
                acc.add(s instanceof RefinedLoopSpec ? burnSrcLoop(path,(RefinedLoopSpec)s)
                        : burnSrcSegment(path,(RefinedSegmentSpec)s));
                return compareSegmentLists(path,
                        src.subList(1,src.size()), edi.subList(1,edi.size()), target, acc);
            } else if (en.equals(tn)) {
                acc.add(t instanceof RefinedLoopSpec ? burnTargetLoop(path,(RefinedLoopSpec)t)
                        : burnTargetSegment(path,(RefinedSegmentSpec)t));
                return compareSegmentLists(path,
                        src, edi.subList(1,edi.size()), target.subList(1,target.size()), acc);
            } else {
                acc.add(new SimpleSegmentDifference(
                        path,
                        nameOf(e), canonicalNameOf(e),
                        new SimpleEntry<>(false,false),
                        new SimpleEntry<>(isRequired(s), isRequired(t)),
                        Optional.empty(),
                        Collections.emptyList()
                ));
                return compareSegmentLists(path, src, edi.subList(1,edi.size()), target, acc);
            }
        }
        // Two-list cases
        if (src.isEmpty() && !edi.isEmpty() && !target.isEmpty()) {
            RefinedSingleOrLoopSegmentSpec t = target.get(0);
            acc.add(t instanceof RefinedLoopSpec ? burnTargetLoop(path,(RefinedLoopSpec)t)
                    : burnTargetSegment(path,(RefinedSegmentSpec)t));
            return compareSegmentLists(path, src, edi.subList(1,edi.size()), target.subList(1,target.size()), acc);
        }
        if (!src.isEmpty() && !edi.isEmpty() && target.isEmpty()) {
            RefinedSingleOrLoopSegmentSpec s = src.get(0);
            acc.add(s instanceof RefinedLoopSpec ? burnSrcLoop(path,(RefinedLoopSpec)s)
                    : burnSrcSegment(path,(RefinedSegmentSpec)s));
            return compareSegmentLists(path, src.subList(1,src.size()), edi.subList(1,edi.size()), target, acc);
        }
        if (src.isEmpty() && !edi.isEmpty() && target.isEmpty()) {
            RefinedSingleOrLoopSegmentSpec e = edi.get(0);
            acc.add(e instanceof RefinedLoopSpec ? burnEdiLoop(path,(RefinedLoopSpec)e)
                    : burnEdiSegment(path,e));
            return compareSegmentLists(path, src, edi.subList(1,edi.size()), target, acc);
        }
        return acc;
    }

    private static SimpleSegmentDifference compareTwoSegments(
            Path path, RefinedSegmentSpec src, RefinedSegmentSpec edi, RefinedSegmentSpec tgt
    ) {
        return new SimpleSegmentDifference(
                path, src.getName(), src.getCanonicalName(),
                new SimpleEntry<>(true,true),
                new SimpleEntry<>(src.isRequired(), tgt.isRequired()),
                Optional.ofNullable(!src.getAssertions().equals(tgt.getAssertions())
                        ? new SimpleEntry<>(src.getAssertions(), tgt.getAssertions()) : null),
                compareSegmentFields(path.dot(src.getCanonicalName()), src.getFields(), edi.getFields(), tgt.getFields())
        );
    }

    private static LoopSegmentDifference compareTwoLoops(
            Path path, RefinedLoopSpec src, RefinedLoopSpec edi, RefinedLoopSpec tgt
    ) {
        return new LoopSegmentDifference(
                path, src.getName(), src.getCanonicalName(),
                new SimpleEntry<>(true,true),
                new SimpleEntry<>(src.isRequired(), tgt.isRequired()),
                Optional.ofNullable(!src.getAssertions().equals(tgt.getAssertions())
                        ? new SimpleEntry<>(src.getAssertions(), tgt.getAssertions()) : null),
                compareSegmentFields(path.dot(src.getCanonicalName()), src.getFields(), edi.getFields(), tgt.getFields()),
                Optional.ofNullable(!Objects.equals(src.getMinRepeats(), tgt.getMinRepeats())
                        ? new SimpleEntry<>(src.getMinRepeats(), tgt.getMinRepeats()) : null),
                Optional.ofNullable(!Objects.equals(src.getMaxRepeats(), tgt.getMaxRepeats())
                        ? new SimpleEntry<>(src.getMaxRepeats(), tgt.getMaxRepeats()) : null),
                compareSegmentLists(path.dot(src.getCanonicalName()), src.getBody(), edi.getBody(), tgt.getBody(), new ArrayList<>()),
                Optional.empty()
        );
    }

    private static SimpleSegmentDifference burnEdiSegment(Path path, RefinedSingleOrLoopSegmentSpec e) {
        return new SimpleSegmentDifference(
                path, nameOf(e), canonicalNameOf(e),
                new SimpleEntry<>(false,false),
                new SimpleEntry<>(isRequired(e), false),
                Optional.empty(), burnEdiFields(path,e)
        );
    }
    private static LoopSegmentDifference burnEdiLoop(Path path, RefinedLoopSpec e) {
        return new LoopSegmentDifference(
                path, e.getName(), e.getCanonicalName(),
                new SimpleEntry<>(false,false),
                new SimpleEntry<>(e.isRequired(), false),
                Optional.empty(), burnEdiFields(path,e), Optional.empty(), Optional.empty(), Collections.emptyList(), Optional.empty()
        );
    }

    private static SimpleSegmentDifference burnSrcSegment(Path path, RefinedSegmentSpec s) {
        return new SimpleSegmentDifference(
                path, s.getName(), s.getCanonicalName(),
                new SimpleEntry<>(true,false),
                new SimpleEntry<>(s.isRequired(), false),
                Optional.empty(), burnSegmentFields(path.dot(s.getCanonicalName()), s.getFields(), true)
        );
    }
    private static LoopSegmentDifference burnSrcLoop(Path path, RefinedLoopSpec s) {
        return new LoopSegmentDifference(
                path, s.getName(), s.getCanonicalName(),
                new SimpleEntry<>(true,false),
                new SimpleEntry<>(s.isRequired(), false),
                Optional.empty(), burnSegmentFields(path.dot(s.getCanonicalName()), s.getFields(), true), Optional.empty(), Optional.empty(), Collections.emptyList(), Optional.empty()
        );
    }

    private static SimpleSegmentDifference burnTargetSegment(Path path, RefinedSegmentSpec t) {
        return new SimpleSegmentDifference(
                path, t.getName(), t.getCanonicalName(),
                new SimpleEntry<>(false,true),
                new SimpleEntry<>(false, t.isRequired()),
                Optional.empty(), burnSegmentFields(path.dot(t.getCanonicalName()), t.getFields(), false)
        );
    }
    private static LoopSegmentDifference burnTargetLoop(Path path, RefinedLoopSpec t) {
        return new LoopSegmentDifference(
                path, t.getName(), t.getCanonicalName(),
                new SimpleEntry<>(false,true),
                new SimpleEntry<>(false, t.isRequired()),
                Optional.empty(), burnSegmentFields(path.dot(t.getCanonicalName()), t.getFields(), false), Optional.empty(), Optional.empty(), Collections.emptyList(), Optional.empty()
        );
    }

    private static List<FieldDifference> compareSegmentFields(
            Path path,
            List<? extends RefinedFieldSpec> src,
            List<? extends RefinedFieldSpec> edi,
            List<? extends RefinedFieldSpec> target
    ) {
        List<FieldDifference> acc = new ArrayList<>();
        int i = 0, j = 0, k = 0;
        int sLen = src.size(), eLen = edi.size(), tLen = target.size();

        while (i < sLen && j < eLen && k < tLen) {
            RefinedFieldSpec s = src.get(i), e = edi.get(j), t = target.get(k);
            String sn = canonicalFieldNameOf(s),
                    en = canonicalFieldNameOf(e),
                    tn = canonicalFieldNameOf(t);

            if (sn.equals(en) && en.equals(tn)) {
                // 3-way match
                if (s instanceof RefinedCompositeFieldSpec) {
                    acc.add(compareTwoCompositeFields(path, (RefinedCompositeFieldSpec)s,
                            (RefinedCompositeFieldSpec)e,
                            (RefinedCompositeFieldSpec)t));
                } else {
                    acc.add(compareTwoSingleFields(path,
                            (RefinedSingleFieldSpec)s,
                            (RefinedSingleFieldSpec)t));
                }
                i++; j++; k++;
            }
            else if (sn.equals(en)) {
                // source+EDI match, target missing
                acc.add(burnSingleOrComposite(path, s, true));
                i++; j++;
            }
            else if (en.equals(tn)) {
                // EDI+target match, source missing
                acc.add(burnSingleOrComposite(path, t, false));
                j++; k++;
            }
            else {
                // no one lines up → report unexpected EDI field
                acc.add(new SingleFieldDifference(
                        path,
                        fieldNameOf(e),
                        canonicalFieldNameOf(e),
                        new SimpleEntry<>(false,false),
                        new SimpleEntry<>(isFieldRequired(s), isFieldRequired(t)),
                        Optional.empty(), Optional.empty(), Optional.empty(), Optional.empty(), Optional.empty()
                ));
                j++;
            }
        }

        // handle leftovers: src only
        while (i < sLen) {
            acc.add(burnSingleOrComposite(path, src.get(i), true));
            i++;
        }
        // edi only
        while (j < eLen) {
            acc.add(burnSingleOrComposite(path, edi.get(j), false));
            j++;
        }
        // target only
        while (k < tLen) {
            acc.add(burnSingleOrComposite(path, target.get(k), false));
            k++;
        }

        return acc;
    }


    private static SingleFieldDifference compareTwoSingleFields(
            Path path, RefinedSingleFieldSpec s, RefinedSingleFieldSpec t
    ) {
        return new SingleFieldDifference(
                path, s.getName(), s.getCanonicalName(),
                new SimpleEntry<>(true,true),
                new SimpleEntry<>(s.isRequired(), t.isRequired()),
                Optional.ofNullable(!Objects.equals(s.getDataType(), t.getDataType())?new SimpleEntry<>(s.getDataType(),t.getDataType()):null),
                Optional.ofNullable(!Objects.equals(s.getFormat(), t.getFormat())?new SimpleEntry<>(s.getFormat(),t.getFormat()):null),
                Optional.ofNullable(!Objects.equals(s.getElementId(), t.getElementId())?new SimpleEntry<>(s.getElementId(),t.getElementId()):null),
                Optional.ofNullable(!Objects.equals(s.getValidValues(), t.getValidValues())?new SimpleEntry<>(s.getValidValues(),t.getValidValues()):null),
                Optional.ofNullable(!Objects.equals(s.getValidValuesRef(), t.getValidValuesRef())?new SimpleEntry<>(s.getValidValuesRef(),t.getValidValuesRef()):null)
        );
    }

    private static CompositeFieldDifference compareTwoCompositeFields(
            Path path, RefinedCompositeFieldSpec s, RefinedCompositeFieldSpec e, RefinedCompositeFieldSpec t
    ) {
        return new CompositeFieldDifference(
                path, s.getName(), s.getCanonicalName(),
                new SimpleEntry<>(true,true),
                new SimpleEntry<>(s.isRequired(),t.isRequired()),
                compareSegmentFields(path.dot(s.getCanonicalName()), s.getComponents(), e.getComponents(), t.getComponents())
        );
    }

    private static FieldDifference burnSingleOrComposite(Path path, RefinedFieldSpec f, boolean isSrc){
        return f instanceof RefinedSingleFieldSpec
                ? burnSingleField(path,(RefinedSingleFieldSpec)f,isSrc)
                : burnCompositeField(path,(RefinedCompositeFieldSpec)f,isSrc);
    }

    private static List<FieldDifference> burnEdiFields(Path path, RefinedSingleOrLoopSegmentSpec e){
        return e.getFields().stream()
                .map(f->burnSingleOrComposite(path,f,false))
                .collect(Collectors.toList());
    }

    private static List<FieldDifference> burnSegmentFields(Path path, List<? extends RefinedFieldSpec> fs, boolean isSrc){
        return fs.stream()
                .map(f->burnSingleOrComposite(path,f,isSrc))
                .collect(Collectors.toList());
    }

    private static SingleFieldDifference burnSingleField(Path path, RefinedSingleFieldSpec f, boolean isSrc){
        return new SingleFieldDifference(
                path, f.getName(), f.getCanonicalName(),
                new SimpleEntry<>(isSrc,!isSrc),
                new SimpleEntry<>(isSrc?f.isRequired():false, isSrc?false:f.isRequired()),
                Optional.empty(),Optional.empty(),Optional.empty(),Optional.empty(),Optional.empty()
        );
    }

    private static CompositeFieldDifference burnCompositeField(Path path, RefinedCompositeFieldSpec f, boolean isSrc){
        return new CompositeFieldDifference(
                path, f.getName(), f.getCanonicalName(),
                new SimpleEntry<>(isSrc,!isSrc),
                new SimpleEntry<>(isSrc?f.isRequired():false, isSrc?false:f.isRequired()),
                Collections.emptyList()
        );
    }

    // Helpers
    private static boolean isRequired(RefinedSingleOrLoopSegmentSpec x){ return x.isRequired(); }
    private static boolean isFieldRequired(RefinedFieldSpec f){ return f.isRequired(); }
    private static String nameOf(RefinedSingleOrLoopSegmentSpec x){ return x.getName(); }
    private static String rawCName(RefinedSingleOrLoopSegmentSpec x){ return x.getCanonicalName(); }
    private static String canonicalNameOf(RefinedSingleOrLoopSegmentSpec x){
        String cn = x.getCanonicalName();
        if(x instanceof RefinedLoopSpec){ RefinedLoopSpec l=(RefinedLoopSpec)x;
            if("HL".equals(cn) && !l.getDescription().isEmpty()) return cn+"["+l.getDescription()+"]";
        }
        return cn;
    }
    private static String fieldNameOf(RefinedFieldSpec f){ return f.getName(); }
    private static String canonicalFieldNameOf(RefinedFieldSpec f){ return f.getCanonicalName(); }
}
