/*
 * This file was automatically generated by EvoSuite
 * Sat May 16 16:17:34 UTC 2020
 */

package org.xwiki.notifications.internal.email;

import org.junit.Test;
import static org.junit.Assert.*;
import static org.evosuite.shaded.org.mockito.Mockito.*;
import static org.evosuite.runtime.EvoAssertions.*;
import ch.qos.logback.classic.Logger;
import com.xpn.xwiki.XWikiContext;
import com.xpn.xwiki.doc.merge.MergeConfiguration;
import com.xpn.xwiki.store.hibernate.HibernateSessionFactory;
import com.xpn.xwiki.store.migration.DataMigrationManager;
import java.util.Locale;
import javax.inject.Provider;
import javax.mail.internet.MimeMessage;
import org.apache.commons.collections.ExtendedProperties;
import org.evosuite.runtime.EvoRunner;
import org.evosuite.runtime.EvoRunnerParameters;
import org.evosuite.runtime.ViolatedAssumptionAnswer;
import org.hibernate.loader.custom.sql.SQLCustomQuery;
import org.junit.runner.RunWith;
import org.slf4j.helpers.NOPLogger;
import org.xwiki.bridge.DocumentAccessBridge;
import org.xwiki.component.manager.ComponentManager;
import org.xwiki.context.Execution;
import org.xwiki.logging.LoggerManager;
import org.xwiki.model.reference.DocumentReferenceResolver;
import org.xwiki.model.reference.EntityReferenceSerializer;
import org.xwiki.notifications.CompositeEvent;
import org.xwiki.notifications.internal.email.PeriodicMimeMessageIterator;
import org.xwiki.wiki.descriptor.WikiDescriptorManager;

@RunWith(EvoRunner.class) @EvoRunnerParameters(useVFS = true, useJEE = true) 
public class AbstractMimeMessageIterator_ESTest extends AbstractMimeMessageIterator_ESTest_scaffolding {

  @Test(timeout = 4000)
  public void test0()  throws Throwable  {
      PeriodicMimeMessageIterator periodicMimeMessageIterator0 = new PeriodicMimeMessageIterator();
      DocumentAccessBridge documentAccessBridge0 = mock(DocumentAccessBridge.class, new ViolatedAssumptionAnswer());
      Logger logger0 = (Logger)SQLCustomQuery.log;
      EntityReferenceSerializer<MimeMessage> entityReferenceSerializer0 = (EntityReferenceSerializer<MimeMessage>) mock(EntityReferenceSerializer.class, new ViolatedAssumptionAnswer());
      DocumentReferenceResolver<CompositeEvent> documentReferenceResolver0 = (DocumentReferenceResolver<CompositeEvent>) mock(DocumentReferenceResolver.class, new ViolatedAssumptionAnswer());
      DocumentReferenceResolver<String> documentReferenceResolver1 = (DocumentReferenceResolver<String>) mock(DocumentReferenceResolver.class, new ViolatedAssumptionAnswer());
      EntityReferenceSerializer<MimeMessage> entityReferenceSerializer1 = (EntityReferenceSerializer<MimeMessage>) mock(EntityReferenceSerializer.class, new ViolatedAssumptionAnswer());
      EntityReferenceSerializer<MimeMessage> entityReferenceSerializer2 = (EntityReferenceSerializer<MimeMessage>) mock(EntityReferenceSerializer.class, new ViolatedAssumptionAnswer());
      NOPLogger nOPLogger0 = NOPLogger.NOP_LOGGER;
      ComponentManager componentManager0 = mock(ComponentManager.class, new ViolatedAssumptionAnswer());
      Locale locale0 = Locale.ROOT;
      XWikiContext xWikiContext0 = new XWikiContext();
      MergeConfiguration mergeConfiguration0 = new MergeConfiguration();
      Provider<String> provider0 = (Provider<String>) mock(Provider.class, new ViolatedAssumptionAnswer());
      DataMigrationManager dataMigrationManager0 = mock(DataMigrationManager.class, new ViolatedAssumptionAnswer());
      Execution execution0 = mock(Execution.class, new ViolatedAssumptionAnswer());
      LoggerManager loggerManager0 = mock(LoggerManager.class, new ViolatedAssumptionAnswer());
      HibernateSessionFactory hibernateSessionFactory0 = mock(HibernateSessionFactory.class, new ViolatedAssumptionAnswer());
      Provider<String> provider1 = (Provider<String>) mock(Provider.class, new ViolatedAssumptionAnswer());
      DocumentReferenceResolver<Object> documentReferenceResolver2 = (DocumentReferenceResolver<Object>) mock(DocumentReferenceResolver.class, new ViolatedAssumptionAnswer());
      WikiDescriptorManager wikiDescriptorManager0 = mock(WikiDescriptorManager.class, new ViolatedAssumptionAnswer());
      ExtendedProperties extendedProperties0 = new ExtendedProperties();
      periodicMimeMessageIterator0.iterator();
      // Undeclared exception!
      periodicMimeMessageIterator0.next();
  }
}
