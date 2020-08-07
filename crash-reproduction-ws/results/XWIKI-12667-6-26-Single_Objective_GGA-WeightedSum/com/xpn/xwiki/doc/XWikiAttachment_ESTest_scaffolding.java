/**
 * Scaffolding file used to store all the setups needed to run 
 * tests automatically generated by EvoSuite
 * Mon Mar 30 18:38:15 UTC 2020
 */

package com.xpn.xwiki.doc;

import org.evosuite.runtime.annotation.EvoSuiteClassExclude;
import org.junit.BeforeClass;
import org.junit.Before;
import org.junit.After;

import static org.evosuite.shaded.org.mockito.Mockito.*;
@EvoSuiteClassExclude
public class XWikiAttachment_ESTest_scaffolding {

  @org.junit.Rule 
  public org.evosuite.runtime.vnet.NonFunctionalRequirementRule nfr = new org.evosuite.runtime.vnet.NonFunctionalRequirementRule();

  private org.evosuite.runtime.thread.ThreadStopper threadStopper =  new org.evosuite.runtime.thread.ThreadStopper (org.evosuite.runtime.thread.KillSwitchHandler.getInstance(), 3000);


  @BeforeClass 
  public static void initEvoSuiteFramework() { 
    org.evosuite.runtime.RuntimeSettings.className = "com.xpn.xwiki.doc.XWikiAttachment"; 
    org.evosuite.runtime.GuiSupport.initialize(); 
    org.evosuite.runtime.RuntimeSettings.maxNumberOfIterationsPerLoop = 10000; 
    org.evosuite.runtime.RuntimeSettings.mockSystemIn = true; 
    org.evosuite.runtime.Runtime.getInstance().resetRuntime(); 
    try { initMocksToAvoidTimeoutsInTheTests(); } catch(ClassNotFoundException e) {} 
  } 

  @Before 
  public void initTestCase(){ 
    threadStopper.storeCurrentThreads();
    threadStopper.startRecordingTime();
    org.evosuite.runtime.GuiSupport.setHeadless(); 
    org.evosuite.runtime.Runtime.getInstance().resetRuntime(); 
    org.evosuite.runtime.agent.InstrumentingAgent.activate(); 
  } 

  @After 
  public void doneWithTestCase(){ 
    threadStopper.killAndJoinClientThreads();
    org.evosuite.runtime.agent.InstrumentingAgent.deactivate(); 
    org.evosuite.runtime.GuiSupport.restoreHeadlessMode(); 
  } 


  private static void initializeClasses() {
    org.evosuite.runtime.classhandling.ClassStateSupport.initializeClasses(XWikiAttachment_ESTest_scaffolding.class.getClassLoader() ,
      "com.xpn.xwiki.XWikiException",
      "org.apache.commons.collections4.map.AbstractLinkedMap",
      "org.xwiki.rendering.block.XDOM",
      "org.suigeneris.jrcs.rcs.Version",
      "org.apache.commons.collections4.KeyValue",
      "org.apache.tika.parser.external.CompositeExternalParser",
      "org.apache.tika.mime.MediaTypeRegistry",
      "org.apache.tika.parser.mp3.MpegStream",
      "org.apache.commons.lang3.StringUtils",
      "com.googlecode.mp4parser.AbstractBox",
      "org.apache.tika.parser.isatab.ISArchiveParser",
      "org.apache.tika.metadata.Property",
      "org.apache.velocity.context.Context",
      "org.apache.tika.parser.font.TrueTypeParser",
      "org.xwiki.component.manager.CompatibilityComponentManager",
      "org.apache.tika.io.TailStream",
      "org.apache.tika.sax.xpath.SubtreeMatcher",
      "org.apache.tika.parser.dif.DIFParser",
      "org.apache.tika.parser.grib.GribParser",
      "org.xwiki.model.reference.ObjectReferenceResolver",
      "com.xpn.xwiki.web.XWikiMessageTool",
      "org.apache.commons.compress.archivers.StreamingNotSupportedException",
      "com.xpn.xwiki.util.AbstractSimpleClass",
      "org.gagravarr.tika.OggDetector",
      "org.apache.struts.action.ActionForm",
      "org.apache.tika.parser.pkg.PackageParser",
      "org.apache.poi.openxml4j.util.ZipSecureFile",
      "org.apache.tika.io.TaggedInputStream",
      "org.apache.tika.parser.mp4.MP4Parser",
      "org.apache.commons.collections4.map.AbstractHashedMap",
      "org.gagravarr.tika.VorbisParser",
      "org.xwiki.model.internal.reference.DefaultStringEntityReferenceSerializer",
      "org.xwiki.component.util.DefaultParameterizedType",
      "org.xwiki.context.ExecutionContext",
      "org.xwiki.observation.EventListener",
      "org.apache.tika.parser.mbox.MboxParser",
      "org.apache.tika.parser.iwork.KeynoteContentHandler",
      "org.xwiki.context.ExecutionContextException",
      "com.xpn.xwiki.objects.ElementInterface",
      "org.apache.tika.parser.image.WebPParser",
      "org.apache.tika.detect.CompositeDetector",
      "org.apache.tika.mime.OrClause",
      "org.bouncycastle.cms.CMSException",
      "org.apache.tika.parser.xml.DcXMLParser",
      "org.apache.tika.config.TikaConfig",
      "org.apache.tika.metadata.CreativeCommons",
      "org.apache.tika.sax.ElementMappingContentHandler$TargetElement",
      "org.ccil.cowan.tagsoup.ElementType",
      "com.xpn.xwiki.doc.merge.MergeConfiguration",
      "org.xwiki.rendering.block.Block",
      "com.xpn.xwiki.objects.classes.PropertyClassInterface",
      "org.apache.tika.mime.Clause",
      "com.xpn.xwiki.doc.AttachmentDiff",
      "org.apache.tika.parser.AbstractParser",
      "org.apache.tika.exception.TikaException",
      "org.apache.tika.utils.ServiceLoaderUtils",
      "com.xpn.xwiki.objects.ObjectDiff",
      "com.xpn.xwiki.api.Api",
      "com.googlecode.mp4parser.boxes.apple.AppleCommentBox",
      "com.healthmarketscience.jackcess.util.LinkResolver",
      "com.googlecode.mp4parser.DataSource",
      "org.apache.tika.parser.image.PSDParser",
      "org.apache.tika.parser.xml.FictionBookParser",
      "org.apache.commons.compress.archivers.zip.ZipArchiveInputStream",
      "org.apache.tika.parser.audio.AudioParser",
      "com.googlecode.mp4parser.boxes.apple.AppleDataBox",
      "org.xwiki.model.reference.EntityReferenceResolver",
      "org.apache.tika.parser.microsoft.JackcessParser$IgnoreLinkResolver",
      "org.xwiki.localization.ContextualLocalizationManager",
      "org.apache.tika.mime.MimeTypesReader",
      "org.apache.tika.parser.microsoft.POIFSContainerDetector",
      "org.apache.commons.lang3.exception.CloneFailedException",
      "org.apache.tika.mime.MimeTypeException",
      "org.apache.tika.parser.microsoft.OfficeParser$POIFSDocumentType",
      "org.apache.commons.collections4.map.AbstractHashedMap$HashEntry",
      "org.xwiki.model.reference.WikiReference",
      "org.apache.poi.poifs.filesystem.DocumentInputStream",
      "com.xpn.xwiki.objects.classes.BaseClass",
      "org.xwiki.rendering.parser.ParseException",
      "org.xwiki.display.internal.DocumentDisplayer",
      "org.apache.velocity.context.InternalEventContext",
      "org.apache.poi.poifs.filesystem.Entry",
      "org.apache.commons.compress.compressors.CompressorInputStream",
      "org.apache.tika.parser.audio.MidiParser",
      "org.xwiki.bridge.DocumentModelBridge",
      "org.dom4j.Node",
      "org.apache.pdfbox.exceptions.CryptographyException",
      "org.xwiki.rendering.block.HeaderBlock",
      "org.xwiki.model.reference.SpaceReference",
      "com.xpn.xwiki.objects.classes.ClassInterface",
      "org.apache.tika.parser.microsoft.ooxml.OOXMLParser",
      "org.apache.tika.parser.pkg.ZipContainerDetector$1",
      "com.xpn.xwiki.objects.ObjectInterface",
      "org.apache.velocity.context.InternalHousekeepingContext",
      "org.apache.tika.parser.asm.ClassParser",
      "org.apache.tika.parser.xml.XMLParser",
      "org.apache.tika.parser.html.HtmlParser",
      "org.apache.tika.language.translate.DefaultTranslator",
      "org.apache.tika.utils.ServiceLoaderUtils$1",
      "org.apache.poi.util.NullLogger",
      "org.suigeneris.jrcs.rcs.Archive",
      "org.apache.tika.mime.MimeType$RootXML",
      "org.apache.xmlrpc.common.XmlRpcRequestProcessor",
      "org.xwiki.rendering.listener.HeaderLevel",
      "org.apache.tika.mime.MimeTypesReaderMetKeys",
      "org.ccil.cowan.tagsoup.HTMLSchema",
      "org.apache.commons.collections4.OrderedMap",
      "org.apache.tika.parser.microsoft.OfficeParser",
      "com.googlecode.mp4parser.boxes.apple.AppleNameBox",
      "org.xwiki.model.internal.reference.LocalizedStringEntityReferenceSerializer",
      "org.apache.poi.poifs.filesystem.DirectoryEntry",
      "com.xpn.xwiki.store.XWikiAttachmentStoreInterface",
      "org.apache.tika.parser.dwg.DWGParser",
      "org.apache.poi.util.POILogger",
      "org.apache.tika.metadata.DublinCore",
      "org.apache.tika.parser.AutoDetectParser",
      "com.xpn.xwiki.doc.XWikiDocument$3",
      "com.googlecode.mp4parser.boxes.apple.AppleGenreBox",
      "org.apache.commons.collections4.OrderedMapIterator",
      "org.apache.tika.parser.iptc.IptcAnpaParser",
      "org.xwiki.rendering.transformation.TransformationException",
      "org.apache.commons.collections4.IterableMap",
      "org.apache.commons.codec.binary.Base64InputStream",
      "org.bouncycastle.operator.OperatorException",
      "com.xpn.xwiki.internal.xml.DOMXMLWriter",
      "org.apache.commons.collections4.IterableGet",
      "org.apache.tika.sax.ContentHandlerDecorator",
      "com.xpn.xwiki.doc.XWikiDocument$XWikiAttachmentToRemove",
      "org.xwiki.rendering.block.AbstractBlock",
      "org.apache.commons.lang3.ObjectUtils$Null",
      "org.apache.tika.parser.external.ExternalParser",
      "org.xwiki.query.QueryException",
      "org.apache.tika.parser.crypto.Pkcs7Parser",
      "org.dom4j.io.XMLWriter",
      "org.apache.tika.parser.geo.topic.GeoParser",
      "com.xpn.xwiki.doc.XWikiDocumentArchive",
      "com.xpn.xwiki.doc.XWikiAttachmentContent",
      "org.apache.tika.extractor.EmbeddedDocumentExtractor",
      "org.apache.tika.parser.code.SourceCodeParser",
      "com.xpn.xwiki.web.XWikiURLFactory",
      "org.ccil.cowan.tagsoup.AttributesImpl",
      "com.github.junrar.exception.RarException",
      "org.xwiki.model.reference.DocumentReferenceResolver",
      "org.apache.tika.parser.executable.MachineMetadata",
      "org.apache.tika.parser.microsoft.TNEFParser",
      "org.xwiki.rendering.transformation.RenderingContext",
      "com.xpn.xwiki.doc.XWikiDocumentCompatibilityAspect",
      "com.xpn.xwiki.web.XWikiEngineContext",
      "org.apache.tika.metadata.Office",
      "org.apache.tika.io.TikaInputStream",
      "org.apache.commons.exec.ExecuteStreamHandler",
      "org.apache.tika.sax.xpath.CompositeMatcher",
      "org.apache.tika.parser.video.FLVParser",
      "org.xwiki.rendering.renderer.printer.WikiPrinter",
      "com.xpn.xwiki.api.DocumentSection",
      "org.apache.tika.parser.external.ExternalParsersFactory",
      "org.apache.commons.compress.compressors.CompressorException",
      "org.dom4j.Branch",
      "org.apache.tika.mime.MimeTypesReader$ClauseRecord",
      "com.coremedia.iso.boxes.Box",
      "com.xpn.xwiki.objects.BaseElement",
      "org.apache.tika.parser.epub.EpubParser",
      "org.apache.tika.metadata.TikaMimeKeys",
      "org.xwiki.rendering.syntax.SyntaxFactory",
      "org.apache.commons.codec.binary.BaseNCodecInputStream",
      "com.googlecode.mp4parser.boxes.apple.AppleAlbumBox",
      "org.apache.tika.parser.chm.ChmParser",
      "org.xwiki.model.reference.ObjectReference",
      "org.apache.tika.parser.iwork.PagesContentHandler",
      "org.apache.tika.parser.pkg.ZipContainerDetector",
      "org.apache.tika.mime.MimeTypes",
      "org.xwiki.model.reference.AttachmentReference",
      "org.dom4j.Element",
      "org.apache.tika.metadata.PropertyTypeException",
      "org.xwiki.display.internal.Displayer",
      "org.apache.commons.collections4.OrderedIterator",
      "org.apache.tika.sax.xpath.NamedAttributeMatcher",
      "org.xwiki.rendering.internal.parser.MissingParserException",
      "org.apache.tika.metadata.TikaCoreProperties$EmbeddedResourceType",
      "org.apache.tika.parser.mbox.OutlookPSTParser",
      "com.xpn.xwiki.doc.MetaDataDiff",
      "org.xwiki.model.reference.EntityReference",
      "org.apache.tika.sax.OfflineContentHandler",
      "org.apache.james.mime4j.MimeException",
      "org.apache.tika.parser.code.SourceCodeParser$1",
      "org.apache.tika.metadata.TikaMetadataKeys",
      "org.apache.commons.io.input.ReaderInputStream",
      "com.xpn.xwiki.doc.XWikiAttachment",
      "org.apache.tika.parser.external.ExternalParsersConfigReader",
      "com.xpn.xwiki.validation.XWikiValidationStatus",
      "com.xpn.xwiki.util.Util",
      "com.googlecode.mp4parser.boxes.apple.AppleArtist2Box",
      "com.xpn.xwiki.objects.BaseCollection",
      "org.apache.tika.metadata.Metadata",
      "com.xpn.xwiki.criteria.impl.RevisionCriteria",
      "org.apache.tika.parser.iwork.IWorkPackageParser$IWORKDocumentType",
      "com.xpn.xwiki.web.XWikiForm",
      "org.apache.commons.compress.archivers.ArchiveInputStream",
      "org.apache.commons.compress.archivers.zip.UnsupportedZipFeatureException",
      "org.xwiki.model.EntityType",
      "com.xpn.xwiki.doc.XWikiLink",
      "org.xwiki.rendering.block.MetaDataBlock",
      "org.apache.tika.parser.gdal.GDALParser",
      "org.apache.tika.sax.xpath.NamedElementMatcher",
      "org.apache.tika.detect.MagicDetector",
      "com.xpn.xwiki.web.XWikiRequest",
      "org.apache.velocity.context.AbstractContext",
      "org.apache.tika.parser.CompositeParser",
      "org.gagravarr.tika.SpeexParser",
      "com.xpn.xwiki.objects.classes.PropertyClass",
      "org.apache.poi.util.StringUtil",
      "org.apache.velocity.VelocityContext",
      "org.apache.xmlrpc.common.XmlRpcController",
      "org.apache.tika.metadata.OfficeOpenXMLExtended",
      "org.apache.tika.io.CloseShieldInputStream",
      "org.apache.tika.metadata.Geographic",
      "org.apache.tika.parser.microsoft.JackcessParser",
      "org.apache.tika.parser.DefaultParser",
      "com.xpn.xwiki.objects.PropertyInterface",
      "org.apache.tika.sax.xpath.Matcher",
      "org.apache.velocity.context.InternalContextBase",
      "com.healthmarketscience.jackcess.impl.CodecProvider",
      "org.suigeneris.jrcs.diff.DifferentiationFailedException",
      "org.ccil.cowan.tagsoup.Schema",
      "org.apache.tika.parser.external.ExternalParser$1",
      "org.apache.commons.collections4.MapIterator",
      "org.apache.tika.parser.external.ExternalParser$2",
      "org.dom4j.Document",
      "org.apache.tika.io.ProxyInputStream",
      "org.apache.tika.config.ServiceLoader",
      "org.apache.tika.parser.external.ExternalParser$3",
      "org.apache.tika.mime.MagicMatch",
      "org.xwiki.component.manager.ComponentLookupException",
      "org.apache.tika.mime.MediaType",
      "org.ccil.cowan.tagsoup.HTMLModels",
      "org.apache.tika.mime.Magic",
      "com.xpn.xwiki.store.XWikiVersioningStoreInterface",
      "org.apache.tika.parser.txt.TXTParser",
      "com.xpn.xwiki.user.api.XWikiUser",
      "org.suigeneris.jrcs.util.ToString",
      "org.apache.tika.sax.xpath.XPathParser",
      "org.suigeneris.jrcs.diff.DiffException",
      "org.apache.tika.parser.EmptyParser",
      "org.apache.tika.config.LoadErrorHandler",
      "org.apache.tika.utils.DateUtils",
      "org.apache.tika.metadata.TIFF",
      "org.apache.tika.parser.executable.ExecutableParser",
      "org.gagravarr.tika.OggParser",
      "com.xpn.xwiki.objects.BaseProperty",
      "com.xpn.xwiki.doc.merge.MergeResult",
      "org.apache.tika.metadata.OfficeOpenXMLCore",
      "org.apache.tika.parser.pdf.PDFParserConfig",
      "org.apache.tika.detect.Detector",
      "org.apache.tika.parser.jpeg.JpegParser",
      "org.aspectj.lang.NoAspectBoundException",
      "org.apache.tika.metadata.TikaCoreProperties",
      "org.apache.tika.parser.pkg.PackageParser$SevenZWrapper",
      "org.apache.tika.parser.jdbc.SQLite3Parser",
      "org.xwiki.model.reference.DocumentReference",
      "org.apache.tika.parser.odf.OpenDocumentParser",
      "com.xpn.xwiki.XWikiContext",
      "org.apache.tika.config.LoadErrorHandler$2",
      "org.apache.tika.config.LoadErrorHandler$1",
      "org.gagravarr.tika.OpusParser",
      "com.xpn.xwiki.doc.XWikiDocument",
      "org.apache.tika.config.LoadErrorHandler$3",
      "org.apache.tika.detect.DefaultDetector",
      "com.googlecode.mp4parser.boxes.apple.AppleTrackAuthorBox",
      "org.apache.tika.mime.MimeType",
      "org.apache.poi.util.POILogFactory",
      "org.apache.jempbox.xmp.XMPSchemaDublinCore",
      "org.apache.tika.parser.netcdf.NetCDFParser",
      "org.apache.tika.parser.Parser",
      "com.xpn.xwiki.internal.render.OldRendering",
      "org.xwiki.model.internal.reference.AbstractStringEntityReferenceSerializer",
      "org.apache.tika.parser.ocr.TesseractOCRConfig",
      "org.bouncycastle.operator.OperatorCreationException",
      "org.apache.tika.sax.xpath.ChildMatcher",
      "org.apache.tika.metadata.XMP",
      "org.apache.commons.collections4.map.LRUMap",
      "org.suigeneris.jrcs.rcs.InvalidVersionNumberException",
      "org.apache.xmlrpc.server.XmlRpcServer",
      "com.xpn.xwiki.store.XWikiStoreInterface",
      "org.apache.commons.collections4.Get",
      "org.xwiki.rendering.syntax.Syntax",
      "org.apache.tika.parser.font.AdobeFontMetricParser",
      "com.xpn.xwiki.web.Utils",
      "org.apache.tika.parser.rtf.RTFParser",
      "org.apache.tika.metadata.ClimateForcast",
      "org.xwiki.model.internal.reference.LocalStringEntityReferenceSerializer",
      "com.googlecode.mp4parser.boxes.apple.AppleArtistBox",
      "org.apache.pdfbox.io.RandomAccess",
      "org.apache.tika.metadata.Property$ValueType",
      "com.xpn.xwiki.web.XWikiResponse",
      "org.xwiki.rendering.transformation.TransformationContext",
      "org.apache.tika.parser.image.TiffParser",
      "org.apache.tika.parser.mail.RFC822Parser",
      "org.apache.poi.poifs.filesystem.DocumentEntry",
      "org.xwiki.model.reference.EntityReferenceSerializer",
      "org.apache.tika.parser.feed.FeedParser",
      "org.apache.tika.parser.microsoft.OldExcelParser",
      "org.apache.tika.mime.AndClause",
      "org.apache.commons.compress.archivers.ArchiveException",
      "org.apache.poi.openxml4j.exceptions.InvalidFormatException",
      "org.gagravarr.tika.OggAudioParser",
      "org.apache.poi.openxml4j.exceptions.OpenXML4JException",
      "com.coremedia.iso.boxes.Container",
      "com.xpn.xwiki.api.Document",
      "org.apache.tika.parser.ParsingReader",
      "org.apache.tika.parser.hdf.HDFParser",
      "com.xpn.xwiki.doc.XWikiAttachmentArchive",
      "org.apache.tika.parser.mat.MatParser",
      "org.xwiki.model.reference.ObjectPropertyReference",
      "org.gagravarr.tika.FlacParser",
      "org.xwiki.component.manager.ComponentManager",
      "org.apache.tika.parser.epub.EpubContentParser",
      "org.apache.tika.parser.ocr.TesseractOCRParser$CompositeImageParser",
      "org.suigeneris.jrcs.diff.delta.Delta",
      "org.apache.poi.openxml4j.util.ZipSecureFile$ThresholdInputStream",
      "org.xwiki.model.reference.AttachmentReferenceResolver",
      "org.apache.sis.storage.UnsupportedStorageException",
      "org.apache.james.mime4j.parser.ContentHandler",
      "com.xpn.xwiki.XWiki",
      "com.xpn.xwiki.internal.xml.XMLWriter",
      "org.apache.tika.parser.iwork.NumbersContentHandler",
      "org.apache.pdfbox.io.RandomAccessRead",
      "org.apache.tika.metadata.Property$PropertyType",
      "org.apache.tika.mime.Patterns$LengthComparator",
      "com.xpn.xwiki.web.EditForm",
      "org.dom4j.DocumentException",
      "org.apache.tika.parser.odf.OpenDocumentContentParser",
      "org.apache.tika.exception.AccessPermissionException",
      "org.apache.tika.parser.pkg.CompressorParser",
      "org.apache.tika.mime.Patterns",
      "org.suigeneris.jrcs.diff.Revision",
      "org.apache.pdfbox.io.SequentialRead",
      "org.apache.commons.lang3.ObjectUtils",
      "org.apache.tika.parser.image.BPGParser",
      "org.apache.tika.parser.geoinfo.GeographicInformationParser",
      "org.apache.poi.util.LittleEndianInput",
      "org.xwiki.model.reference.LocalDocumentReference",
      "org.apache.tika.parser.pkg.RarParser",
      "org.apache.sis.storage.DataStoreException",
      "com.googlecode.mp4parser.boxes.apple.Utf8AppleDataBox",
      "org.xwiki.rendering.syntax.SyntaxType",
      "org.apache.tika.parser.iwork.IWorkPackageParser",
      "org.apache.tika.metadata.Message",
      "org.xwiki.rendering.block.match.BlockMatcher",
      "com.xpn.xwiki.internal.cache.rendering.RenderingCache",
      "com.xpn.xwiki.objects.BaseObject",
      "org.apache.tika.parser.odf.OpenDocumentMetaParser",
      "org.apache.tika.parser.pdf.AccessChecker",
      "com.xpn.xwiki.doc.rcs.XWikiRCSNodeInfo",
      "org.apache.tika.mime.MimeTypesFactory",
      "org.apache.commons.collections4.BoundedMap",
      "org.apache.tika.io.IOUtils",
      "com.xpn.xwiki.doc.XWikiLock",
      "org.xwiki.rendering.block.CompatibilityBlock",
      "com.xpn.xwiki.objects.CompatibilityObjectInterface",
      "org.apache.tika.language.translate.Translator",
      "org.apache.tika.parser.image.ImageParser",
      "org.apache.commons.collections4.Put",
      "org.apache.tika.metadata.MSOffice",
      "org.apache.tika.parser.pdf.PDFParser",
      "org.apache.jempbox.xmp.XMPSchema",
      "org.xwiki.job.event.status.JobProgressManager",
      "org.apache.tika.Tika",
      "com.xpn.xwiki.plugin.query.XWikiCriteria",
      "org.apache.commons.collections4.map.AbstractLinkedMap$LinkEntry",
      "org.apache.tika.metadata.HttpHeaders",
      "org.apache.tika.exception.EncryptedDocumentException",
      "org.apache.tika.parser.mp3.Mp3Parser",
      "org.apache.tika.parser.external.ExternalParsersConfigReaderMetKeys",
      "org.apache.tika.parser.ocr.TesseractOCRParser"
    );
  } 
  private static void initMocksToAvoidTimeoutsInTheTests() throws ClassNotFoundException { 
    mock(Class.forName("com.xpn.xwiki.doc.XWikiDocument", false, XWikiAttachment_ESTest_scaffolding.class.getClassLoader()));
  }
}
