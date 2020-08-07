/*
 * This file was automatically generated by EvoSuite
 * Thu May 14 14:38:37 UTC 2020
 */

package com.xpn.xwiki.objects.classes;

import org.junit.Test;
import static org.junit.Assert.*;
import static org.evosuite.shaded.org.mockito.Mockito.*;
import static org.evosuite.runtime.EvoAssertions.*;
import com.xpn.xwiki.objects.classes.BaseClass;
import com.xpn.xwiki.objects.classes.PropertyClass;
import com.xpn.xwiki.store.XWikiHibernateBaseStore;
import com.xpn.xwiki.store.XWikiHibernateRecycleBinStore;
import com.xpn.xwiki.store.hibernate.HibernateSessionFactory;
import com.xpn.xwiki.store.migration.DataMigrationManager;
import javax.inject.Provider;
import org.evosuite.runtime.EvoRunner;
import org.evosuite.runtime.EvoRunnerParameters;
import org.evosuite.runtime.ViolatedAssumptionAnswer;
import org.evosuite.runtime.javaee.injection.Injector;
import org.junit.runner.RunWith;
import org.xwiki.context.Execution;
import org.xwiki.logging.LoggerManager;

@RunWith(EvoRunner.class) @EvoRunnerParameters(useVFS = true, useJEE = true) 
public class BaseClass_ESTest extends BaseClass_ESTest_scaffolding {

  @Test(timeout = 4000)
  public void test0()  throws Throwable  {
      BaseClass baseClass0 = new BaseClass();
      baseClass0.flushCache();
      baseClass0.getLongValue("elseif");
      baseClass0.getPropertyNames();
      baseClass0.addTextAreaField("elseif", "elseif", 10003, (-4039), "", "elseif");
      baseClass0.addBooleanField("", "elseif", (String) null);
      baseClass0.setId(381L);
      baseClass0.addStaticListField("j03O~FU", "", 897, true, "", (String) null);
      XWikiHibernateRecycleBinStore xWikiHibernateRecycleBinStore0 = new XWikiHibernateRecycleBinStore();
      DataMigrationManager dataMigrationManager0 = mock(DataMigrationManager.class, new ViolatedAssumptionAnswer());
      Injector.inject(xWikiHibernateRecycleBinStore0, (Class<?>) XWikiHibernateBaseStore.class, "dataMigrationManager", (Object) dataMigrationManager0);
      Execution execution0 = mock(Execution.class, new ViolatedAssumptionAnswer());
      Injector.inject(xWikiHibernateRecycleBinStore0, (Class<?>) XWikiHibernateBaseStore.class, "execution", (Object) execution0);
      LoggerManager loggerManager0 = mock(LoggerManager.class, new ViolatedAssumptionAnswer());
      Injector.inject(xWikiHibernateRecycleBinStore0, (Class<?>) XWikiHibernateBaseStore.class, "loggerManager", (Object) loggerManager0);
      HibernateSessionFactory hibernateSessionFactory0 = mock(HibernateSessionFactory.class, new ViolatedAssumptionAnswer());
      Injector.inject(xWikiHibernateRecycleBinStore0, (Class<?>) XWikiHibernateBaseStore.class, "sessionFactory", (Object) hibernateSessionFactory0);
      Provider<PropertyClass> provider0 = (Provider<PropertyClass>) mock(Provider.class, new ViolatedAssumptionAnswer());
      Injector.inject(xWikiHibernateRecycleBinStore0, (Class<?>) XWikiHibernateBaseStore.class, "xcontextProvider", (Object) provider0);
      Injector.validateBean(xWikiHibernateRecycleBinStore0, (Class<?>) XWikiHibernateRecycleBinStore.class);
      // Undeclared exception!
      baseClass0.addTextAreaField("", "XWiki.XWikiPreferences", 897, 3368, "", "VRpS=Sn`");
  }
}
