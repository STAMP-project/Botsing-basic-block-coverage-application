/*
 * This file was automatically generated by EvoSuite
 * Sat May 16 12:50:33 UTC 2020
 */

package com.xpn.xwiki.store.hibernate.query;

import org.junit.Test;
import static org.junit.Assert.*;
import static org.evosuite.shaded.org.mockito.Mockito.*;
import static org.evosuite.runtime.EvoAssertions.*;
import ch.qos.logback.classic.Logger;
import com.xpn.xwiki.store.hibernate.HibernateSessionFactory;
import com.xpn.xwiki.store.hibernate.query.HqlQueryExecutor;
import org.evosuite.runtime.EvoRunner;
import org.evosuite.runtime.EvoRunnerParameters;
import org.evosuite.runtime.ViolatedAssumptionAnswer;
import org.evosuite.runtime.javaee.injection.Injector;
import org.hibernate.Session;
import org.junit.runner.RunWith;
import org.xwiki.context.Execution;
import org.xwiki.job.event.status.JobProgressManager;
import org.xwiki.query.Query;
import org.xwiki.query.internal.CountDocumentFilter;
import org.xwiki.query.internal.DefaultQuery;
import org.xwiki.security.authorization.ContextualAuthorizationManager;
import ucar.nc2.util.net.URLStreamHandlerFactory;

@RunWith(EvoRunner.class) @EvoRunnerParameters(useVFS = true, useJEE = true) 
public class HqlQueryExecutor_ESTest extends HqlQueryExecutor_ESTest_scaffolding {

  @Test(timeout = 4000)
  public void test0()  throws Throwable  {
      ContextualAuthorizationManager contextualAuthorizationManager0 = mock(ContextualAuthorizationManager.class, new ViolatedAssumptionAnswer());
      Execution execution0 = mock(Execution.class, new ViolatedAssumptionAnswer());
      JobProgressManager jobProgressManager0 = mock(JobProgressManager.class, new ViolatedAssumptionAnswer());
      HibernateSessionFactory hibernateSessionFactory0 = mock(HibernateSessionFactory.class, new ViolatedAssumptionAnswer());
      Session session0 = mock(Session.class, new ViolatedAssumptionAnswer());
      Query query0 = mock(Query.class, new ViolatedAssumptionAnswer());
      Query query1 = mock(Query.class, new ViolatedAssumptionAnswer());
      Query query2 = mock(Query.class, new ViolatedAssumptionAnswer());
      HqlQueryExecutor hqlQueryExecutor0 = new HqlQueryExecutor();
      ContextualAuthorizationManager contextualAuthorizationManager1 = mock(ContextualAuthorizationManager.class, new ViolatedAssumptionAnswer());
      Injector.inject(hqlQueryExecutor0, (Class<?>) HqlQueryExecutor.class, "authorization", (Object) contextualAuthorizationManager1);
      Execution execution1 = mock(Execution.class, new ViolatedAssumptionAnswer());
      Injector.inject(hqlQueryExecutor0, (Class<?>) HqlQueryExecutor.class, "execution", (Object) execution1);
      JobProgressManager jobProgressManager1 = mock(JobProgressManager.class, new ViolatedAssumptionAnswer());
      Injector.inject(hqlQueryExecutor0, (Class<?>) HqlQueryExecutor.class, "progress", (Object) jobProgressManager1);
      HibernateSessionFactory hibernateSessionFactory1 = mock(HibernateSessionFactory.class, new ViolatedAssumptionAnswer());
      Injector.inject(hqlQueryExecutor0, (Class<?>) HqlQueryExecutor.class, "sessionFactory", (Object) hibernateSessionFactory1);
      Injector.validateBean(hqlQueryExecutor0, (Class<?>) HqlQueryExecutor.class);
      DefaultQuery defaultQuery0 = new DefaultQuery("Ta8dWs|*", "Ta8dWs|*", hqlQueryExecutor0);
      CountDocumentFilter countDocumentFilter0 = new CountDocumentFilter();
      Logger logger0 = (Logger)URLStreamHandlerFactory.log;
      Injector.inject(countDocumentFilter0, (Class<?>) CountDocumentFilter.class, "logger", (Object) logger0);
      Injector.validateBean(countDocumentFilter0, (Class<?>) CountDocumentFilter.class);
      Query query3 = defaultQuery0.addFilter(countDocumentFilter0);
      // Undeclared exception!
      hqlQueryExecutor0.createHibernateQuery((Session) null, query3);
  }
}
