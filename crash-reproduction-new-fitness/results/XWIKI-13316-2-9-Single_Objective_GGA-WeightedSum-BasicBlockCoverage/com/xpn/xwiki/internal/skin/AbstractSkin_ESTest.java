/*
 * This file was automatically generated by EvoSuite
 * Thu May 14 16:43:30 UTC 2020
 */

package com.xpn.xwiki.internal.skin;

import org.junit.Test;
import static org.junit.Assert.*;
import static org.evosuite.shaded.org.mockito.Mockito.*;
import static org.evosuite.runtime.EvoAssertions.*;
import ch.qos.logback.classic.Logger;
import com.xpn.xwiki.internal.ReadOnlyXWikiContextProvider;
import com.xpn.xwiki.internal.skin.EnvironmentSkin;
import com.xpn.xwiki.internal.skin.InternalSkinConfiguration;
import com.xpn.xwiki.internal.skin.InternalSkinManager;
import org.evosuite.runtime.EvoRunner;
import org.evosuite.runtime.EvoRunnerParameters;
import org.evosuite.runtime.ViolatedAssumptionAnswer;
import org.evosuite.runtime.javaee.injection.Injector;
import org.hibernate.loader.custom.sql.SQLCustomQuery;
import org.junit.runner.RunWith;
import org.slf4j.event.SubstituteLoggingEvent;
import org.xwiki.context.Execution;
import org.xwiki.environment.Environment;
import org.xwiki.rendering.syntax.SyntaxFactory;

@RunWith(EvoRunner.class) @EvoRunnerParameters(useVFS = true, useJEE = true) 
public class AbstractSkin_ESTest extends AbstractSkin_ESTest_scaffolding {

  @Test(timeout = 4000)
  public void test0()  throws Throwable  {
      Logger logger0 = (Logger)SQLCustomQuery.log;
      Logger logger1 = (Logger)SQLCustomQuery.log;
      SubstituteLoggingEvent substituteLoggingEvent0 = new SubstituteLoggingEvent();
      substituteLoggingEvent0.getLogger();
      ReadOnlyXWikiContextProvider readOnlyXWikiContextProvider0 = new ReadOnlyXWikiContextProvider();
      Execution execution0 = mock(Execution.class, new ViolatedAssumptionAnswer());
      Injector.inject(readOnlyXWikiContextProvider0, (Class<?>) ReadOnlyXWikiContextProvider.class, "execution", (Object) execution0);
      Injector.validateBean(readOnlyXWikiContextProvider0, (Class<?>) ReadOnlyXWikiContextProvider.class);
      EnvironmentSkin environmentSkin0 = new EnvironmentSkin("y+3e&!,jI#yw%~)f0", (InternalSkinManager) null, (InternalSkinConfiguration) null, (org.slf4j.Logger) null, (SyntaxFactory) null, (Environment) null, readOnlyXWikiContextProvider0);
      // Undeclared exception!
      environmentSkin0.getOutputSyntax();
  }
}
