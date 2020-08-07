/*
 * This file was automatically generated by EvoSuite
 * Thu May 14 12:47:55 UTC 2020
 */

package com.xpn.xwiki.store.hibernate.query;

import org.junit.Test;
import static org.junit.Assert.*;
import static org.evosuite.shaded.org.mockito.Mockito.*;
import static org.evosuite.runtime.EvoAssertions.*;
import com.xpn.xwiki.store.hibernate.HibernateSessionFactory;
import com.xpn.xwiki.store.hibernate.query.HqlQueryExecutor;
import java.util.concurrent.LinkedBlockingDeque;
import org.evosuite.runtime.EvoRunner;
import org.evosuite.runtime.EvoRunnerParameters;
import org.evosuite.runtime.ViolatedAssumptionAnswer;
import org.evosuite.runtime.javaee.injection.Injector;
import org.hibernate.Session;
import org.junit.runner.RunWith;
import org.slf4j.event.SubstituteLoggingEvent;
import org.slf4j.helpers.SubstituteLogger;
import org.xwiki.context.Execution;
import org.xwiki.job.event.status.JobProgressManager;
import org.xwiki.query.Query;
import org.xwiki.query.QueryFilter;
import org.xwiki.query.internal.CountDocumentFilter;
import org.xwiki.query.internal.DefaultQuery;
import org.xwiki.security.authorization.ContextualAuthorizationManager;

@RunWith(EvoRunner.class) @EvoRunnerParameters(useVFS = true, useJEE = true) 
public class HqlQueryExecutor_ESTest extends HqlQueryExecutor_ESTest_scaffolding {

  @Test(timeout = 4000)
  public void test0()  throws Throwable  {
      ContextualAuthorizationManager contextualAuthorizationManager0 = mock(ContextualAuthorizationManager.class, new ViolatedAssumptionAnswer());
      Execution execution0 = mock(Execution.class, new ViolatedAssumptionAnswer());
      HqlQueryExecutor hqlQueryExecutor0 = new HqlQueryExecutor();
      ContextualAuthorizationManager contextualAuthorizationManager1 = mock(ContextualAuthorizationManager.class, new ViolatedAssumptionAnswer());
      Injector.inject(hqlQueryExecutor0, (Class<?>) HqlQueryExecutor.class, "authorization", (Object) contextualAuthorizationManager1);
      Execution execution1 = mock(Execution.class, new ViolatedAssumptionAnswer());
      Injector.inject(hqlQueryExecutor0, (Class<?>) HqlQueryExecutor.class, "execution", (Object) execution1);
      JobProgressManager jobProgressManager0 = mock(JobProgressManager.class, new ViolatedAssumptionAnswer());
      Injector.inject(hqlQueryExecutor0, (Class<?>) HqlQueryExecutor.class, "progress", (Object) jobProgressManager0);
      HibernateSessionFactory hibernateSessionFactory0 = mock(HibernateSessionFactory.class, new ViolatedAssumptionAnswer());
      Injector.inject(hqlQueryExecutor0, (Class<?>) HqlQueryExecutor.class, "sessionFactory", (Object) hibernateSessionFactory0);
      Injector.validateBean(hqlQueryExecutor0, (Class<?>) HqlQueryExecutor.class);
      HqlQueryExecutor.isSafeSelect(") has to be < max_interval (");
      DefaultQuery defaultQuery0 = new DefaultQuery(") has to be < max_interval (", ") has to be < max_interval (", hqlQueryExecutor0);
      CountDocumentFilter countDocumentFilter0 = new CountDocumentFilter();
      LinkedBlockingDeque<SubstituteLoggingEvent> linkedBlockingDeque0 = new LinkedBlockingDeque<SubstituteLoggingEvent>();
      SubstituteLogger substituteLogger0 = new SubstituteLogger("IMr?tu1C<c9=C|", linkedBlockingDeque0, false);
      Injector.inject(countDocumentFilter0, (Class<?>) CountDocumentFilter.class, "logger", (Object) substituteLogger0);
      Injector.validateBean(countDocumentFilter0, (Class<?>) CountDocumentFilter.class);
      defaultQuery0.addFilter(countDocumentFilter0);
      Query query0 = defaultQuery0.addFilter((QueryFilter) null);
      hqlQueryExecutor0.checkAllowed(query0);
      HqlQueryExecutor hqlQueryExecutor1 = new HqlQueryExecutor();
      Injector.inject(hqlQueryExecutor1, (Class<?>) HqlQueryExecutor.class, "authorization", (Object) contextualAuthorizationManager0);
      Injector.inject(hqlQueryExecutor1, (Class<?>) HqlQueryExecutor.class, "execution", (Object) execution0);
      JobProgressManager jobProgressManager1 = mock(JobProgressManager.class, new ViolatedAssumptionAnswer());
      Injector.inject(hqlQueryExecutor1, (Class<?>) HqlQueryExecutor.class, "progress", (Object) jobProgressManager1);
      HibernateSessionFactory hibernateSessionFactory1 = mock(HibernateSessionFactory.class, new ViolatedAssumptionAnswer());
      Injector.inject(hqlQueryExecutor1, (Class<?>) HqlQueryExecutor.class, "sessionFactory", (Object) hibernateSessionFactory1);
      Injector.validateBean(hqlQueryExecutor1, (Class<?>) HqlQueryExecutor.class);
      HqlQueryExecutor hqlQueryExecutor2 = new HqlQueryExecutor();
      ContextualAuthorizationManager contextualAuthorizationManager2 = mock(ContextualAuthorizationManager.class, new ViolatedAssumptionAnswer());
      Injector.inject(hqlQueryExecutor2, (Class<?>) HqlQueryExecutor.class, "authorization", (Object) contextualAuthorizationManager2);
      Execution execution2 = mock(Execution.class, new ViolatedAssumptionAnswer());
      Injector.inject(hqlQueryExecutor2, (Class<?>) HqlQueryExecutor.class, "execution", (Object) execution2);
      JobProgressManager jobProgressManager2 = mock(JobProgressManager.class, new ViolatedAssumptionAnswer());
      Injector.inject(hqlQueryExecutor2, (Class<?>) HqlQueryExecutor.class, "progress", (Object) jobProgressManager2);
      HibernateSessionFactory hibernateSessionFactory2 = mock(HibernateSessionFactory.class, new ViolatedAssumptionAnswer());
      Injector.inject(hqlQueryExecutor2, (Class<?>) HqlQueryExecutor.class, "sessionFactory", (Object) hibernateSessionFactory2);
      Injector.validateBean(hqlQueryExecutor2, (Class<?>) HqlQueryExecutor.class);
      // Undeclared exception!
      hqlQueryExecutor2.createHibernateQuery((Session) null, defaultQuery0);
  }
}
