/*
 * This file was automatically generated by EvoSuite
 * Sat May 16 12:49:28 UTC 2020
 */

package com.xpn.xwiki.store.hibernate.query;

import org.junit.Test;
import static org.junit.Assert.*;
import static org.evosuite.shaded.org.mockito.Mockito.*;
import static org.evosuite.runtime.EvoAssertions.*;
import com.xpn.xwiki.store.hibernate.HibernateSessionFactory;
import com.xpn.xwiki.store.hibernate.query.HqlQueryExecutor;
import java.util.PriorityQueue;
import javax.inject.Provider;
import org.evosuite.runtime.EvoRunner;
import org.evosuite.runtime.EvoRunnerParameters;
import org.evosuite.runtime.ViolatedAssumptionAnswer;
import org.evosuite.runtime.javaee.injection.Injector;
import org.hibernate.Session;
import org.junit.runner.RunWith;
import org.slf4j.event.EventRecodingLogger;
import org.slf4j.event.SubstituteLoggingEvent;
import org.slf4j.helpers.SubstituteLogger;
import org.xwiki.context.Execution;
import org.xwiki.job.event.status.JobProgressManager;
import org.xwiki.query.Query;
import org.xwiki.query.internal.CountDocumentFilter;
import org.xwiki.query.internal.DefaultQuery;
import org.xwiki.query.internal.DefaultQueryExecutorManager;
import org.xwiki.query.internal.NoOpQueryFilter;
import org.xwiki.query.internal.SecureQueryExecutorManager;
import org.xwiki.security.authorization.ContextualAuthorizationManager;

@RunWith(EvoRunner.class) @EvoRunnerParameters(useVFS = true, useJEE = true) 
public class HqlQueryExecutor_ESTest extends HqlQueryExecutor_ESTest_scaffolding {

  @Test(timeout = 4000)
  public void test0()  throws Throwable  {
      ContextualAuthorizationManager contextualAuthorizationManager0 = mock(ContextualAuthorizationManager.class, new ViolatedAssumptionAnswer());
      Execution execution0 = mock(Execution.class, new ViolatedAssumptionAnswer());
      JobProgressManager jobProgressManager0 = mock(JobProgressManager.class, new ViolatedAssumptionAnswer());
      HibernateSessionFactory hibernateSessionFactory0 = mock(HibernateSessionFactory.class, new ViolatedAssumptionAnswer());
      Session session0 = mock(Session.class, new ViolatedAssumptionAnswer());
      PriorityQueue<SubstituteLoggingEvent> priorityQueue0 = new PriorityQueue<SubstituteLoggingEvent>();
      SubstituteLogger substituteLogger0 = new SubstituteLogger((String) null, priorityQueue0, false);
      SecureQueryExecutorManager secureQueryExecutorManager0 = new SecureQueryExecutorManager();
      ContextualAuthorizationManager contextualAuthorizationManager1 = mock(ContextualAuthorizationManager.class, new ViolatedAssumptionAnswer());
      Injector.inject(secureQueryExecutorManager0, (Class<?>) SecureQueryExecutorManager.class, "authorization", (Object) contextualAuthorizationManager1);
      DefaultQueryExecutorManager defaultQueryExecutorManager0 = new DefaultQueryExecutorManager();
      Provider<DefaultQuery> provider0 = (Provider<DefaultQuery>) mock(Provider.class, new ViolatedAssumptionAnswer());
      Injector.inject(defaultQueryExecutorManager0, (Class<?>) DefaultQueryExecutorManager.class, "componentManagerProvider", (Object) provider0);
      Provider<NoOpQueryFilter> provider1 = (Provider<NoOpQueryFilter>) mock(Provider.class, new ViolatedAssumptionAnswer());
      Injector.inject(defaultQueryExecutorManager0, (Class<?>) DefaultQueryExecutorManager.class, "namedQueryExecutorProvider", (Object) provider1);
      Injector.validateBean(defaultQueryExecutorManager0, (Class<?>) DefaultQueryExecutorManager.class);
      Injector.inject(secureQueryExecutorManager0, (Class<?>) SecureQueryExecutorManager.class, "defaultQueryExecutorManager", (Object) defaultQueryExecutorManager0);
      Injector.validateBean(secureQueryExecutorManager0, (Class<?>) SecureQueryExecutorManager.class);
      DefaultQuery defaultQuery0 = new DefaultQuery("(|j@Eo!ryVx", (String) null, secureQueryExecutorManager0);
      CountDocumentFilter countDocumentFilter0 = new CountDocumentFilter();
      EventRecodingLogger eventRecodingLogger0 = new EventRecodingLogger(substituteLogger0, priorityQueue0);
      Injector.inject(countDocumentFilter0, (Class<?>) CountDocumentFilter.class, "logger", (Object) eventRecodingLogger0);
      Injector.validateBean(countDocumentFilter0, (Class<?>) CountDocumentFilter.class);
      Query query0 = defaultQuery0.addFilter(countDocumentFilter0);
      HqlQueryExecutor hqlQueryExecutor0 = new HqlQueryExecutor();
      ContextualAuthorizationManager contextualAuthorizationManager2 = mock(ContextualAuthorizationManager.class, new ViolatedAssumptionAnswer());
      Injector.inject(hqlQueryExecutor0, (Class<?>) HqlQueryExecutor.class, "authorization", (Object) contextualAuthorizationManager2);
      Execution execution1 = mock(Execution.class, new ViolatedAssumptionAnswer());
      Injector.inject(hqlQueryExecutor0, (Class<?>) HqlQueryExecutor.class, "execution", (Object) execution1);
      JobProgressManager jobProgressManager1 = mock(JobProgressManager.class, new ViolatedAssumptionAnswer());
      Injector.inject(hqlQueryExecutor0, (Class<?>) HqlQueryExecutor.class, "progress", (Object) jobProgressManager1);
      HibernateSessionFactory hibernateSessionFactory1 = mock(HibernateSessionFactory.class, new ViolatedAssumptionAnswer());
      Injector.inject(hqlQueryExecutor0, (Class<?>) HqlQueryExecutor.class, "sessionFactory", (Object) hibernateSessionFactory1);
      Injector.validateBean(hqlQueryExecutor0, (Class<?>) HqlQueryExecutor.class);
      // Undeclared exception!
      hqlQueryExecutor0.createHibernateQuery(session0, query0);
  }
}
