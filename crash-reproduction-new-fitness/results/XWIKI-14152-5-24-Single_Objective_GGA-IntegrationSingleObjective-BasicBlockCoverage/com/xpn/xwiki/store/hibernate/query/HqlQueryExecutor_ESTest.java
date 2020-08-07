/*
 * This file was automatically generated by EvoSuite
 * Sun May 17 17:13:38 UTC 2020
 */

package com.xpn.xwiki.store.hibernate.query;

import org.junit.Test;
import static org.junit.Assert.*;
import static org.evosuite.shaded.org.mockito.Mockito.*;
import static org.evosuite.runtime.EvoAssertions.*;
import com.xpn.xwiki.store.hibernate.HibernateSessionFactory;
import com.xpn.xwiki.store.hibernate.query.HqlQueryExecutor;
import java.awt.Scrollbar;
import javax.inject.Provider;
import org.evosuite.runtime.EvoRunner;
import org.evosuite.runtime.EvoRunnerParameters;
import org.evosuite.runtime.ViolatedAssumptionAnswer;
import org.evosuite.runtime.javaee.injection.Injector;
import org.hibernate.Session;
import org.hibernate.engine.NamedSQLQueryDefinition;
import org.jgroups.util.TimeScheduler3;
import org.junit.runner.RunWith;
import org.slf4j.helpers.NOPLogger;
import org.xwiki.context.Execution;
import org.xwiki.job.event.status.JobProgressManager;
import org.xwiki.query.Query;
import org.xwiki.query.internal.CountDocumentFilter;
import org.xwiki.query.internal.DefaultQuery;
import org.xwiki.query.internal.DefaultQueryExecutorManager;
import org.xwiki.query.internal.ScriptQuery;
import org.xwiki.query.internal.SecureQueryExecutorManager;
import org.xwiki.security.authorization.ContextualAuthorizationManager;

@RunWith(EvoRunner.class) @EvoRunnerParameters(useVFS = true, useJEE = true) 
public class HqlQueryExecutor_ESTest extends HqlQueryExecutor_ESTest_scaffolding {

  @Test(timeout = 4000)
  public void test0()  throws Throwable  {
      HqlQueryExecutor hqlQueryExecutor0 = new HqlQueryExecutor();
      ContextualAuthorizationManager contextualAuthorizationManager0 = mock(ContextualAuthorizationManager.class, new ViolatedAssumptionAnswer());
      Injector.inject(hqlQueryExecutor0, (Class<?>) HqlQueryExecutor.class, "authorization", (Object) contextualAuthorizationManager0);
      Execution execution0 = mock(Execution.class, new ViolatedAssumptionAnswer());
      Injector.inject(hqlQueryExecutor0, (Class<?>) HqlQueryExecutor.class, "execution", (Object) execution0);
      JobProgressManager jobProgressManager0 = mock(JobProgressManager.class, new ViolatedAssumptionAnswer());
      Injector.inject(hqlQueryExecutor0, (Class<?>) HqlQueryExecutor.class, "progress", (Object) jobProgressManager0);
      HibernateSessionFactory hibernateSessionFactory0 = mock(HibernateSessionFactory.class, new ViolatedAssumptionAnswer());
      Injector.inject(hqlQueryExecutor0, (Class<?>) HqlQueryExecutor.class, "sessionFactory", (Object) hibernateSessionFactory0);
      Injector.validateBean(hqlQueryExecutor0, (Class<?>) HqlQueryExecutor.class);
      SecureQueryExecutorManager secureQueryExecutorManager0 = new SecureQueryExecutorManager();
      ContextualAuthorizationManager contextualAuthorizationManager1 = mock(ContextualAuthorizationManager.class, new ViolatedAssumptionAnswer());
      Injector.inject(secureQueryExecutorManager0, (Class<?>) SecureQueryExecutorManager.class, "authorization", (Object) contextualAuthorizationManager1);
      DefaultQueryExecutorManager defaultQueryExecutorManager0 = new DefaultQueryExecutorManager();
      Provider<TimeScheduler3.Task> provider0 = (Provider<TimeScheduler3.Task>) mock(Provider.class, new ViolatedAssumptionAnswer());
      Injector.inject(defaultQueryExecutorManager0, (Class<?>) DefaultQueryExecutorManager.class, "componentManagerProvider", (Object) provider0);
      Provider<NamedSQLQueryDefinition> provider1 = (Provider<NamedSQLQueryDefinition>) mock(Provider.class, new ViolatedAssumptionAnswer());
      Injector.inject(defaultQueryExecutorManager0, (Class<?>) DefaultQueryExecutorManager.class, "namedQueryExecutorProvider", (Object) provider1);
      Injector.validateBean(defaultQueryExecutorManager0, (Class<?>) DefaultQueryExecutorManager.class);
      Injector.inject(secureQueryExecutorManager0, (Class<?>) SecureQueryExecutorManager.class, "defaultQueryExecutorManager", (Object) defaultQueryExecutorManager0);
      Injector.validateBean(secureQueryExecutorManager0, (Class<?>) SecureQueryExecutorManager.class);
      DefaultQueryExecutorManager defaultQueryExecutorManager1 = new DefaultQueryExecutorManager();
      Provider<Scrollbar> provider2 = (Provider<Scrollbar>) mock(Provider.class, new ViolatedAssumptionAnswer());
      Injector.inject(defaultQueryExecutorManager1, (Class<?>) DefaultQueryExecutorManager.class, "componentManagerProvider", (Object) provider2);
      Provider<ScriptQuery> provider3 = (Provider<ScriptQuery>) mock(Provider.class, new ViolatedAssumptionAnswer());
      Injector.inject(defaultQueryExecutorManager1, (Class<?>) DefaultQueryExecutorManager.class, "namedQueryExecutorProvider", (Object) provider3);
      Injector.validateBean(defaultQueryExecutorManager1, (Class<?>) DefaultQueryExecutorManager.class);
      DefaultQuery defaultQuery0 = new DefaultQuery("k!|36Z!d=QuiM", "k!|36Z!d=QuiM", hqlQueryExecutor0);
      Query query0 = defaultQuery0.setWiki("k!|36Z!d=QuiM");
      CountDocumentFilter countDocumentFilter0 = new CountDocumentFilter();
      NOPLogger nOPLogger0 = NOPLogger.NOP_LOGGER;
      Injector.inject(countDocumentFilter0, (Class<?>) CountDocumentFilter.class, "logger", (Object) nOPLogger0);
      Injector.validateBean(countDocumentFilter0, (Class<?>) CountDocumentFilter.class);
      query0.addFilter(countDocumentFilter0);
      hqlQueryExecutor0.populateParameters((org.hibernate.Query) null, defaultQuery0);
      // Undeclared exception!
      hqlQueryExecutor0.createHibernateQuery((Session) null, defaultQuery0);
  }
}
