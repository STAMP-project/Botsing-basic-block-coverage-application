/*
 * This file was automatically generated by EvoSuite
 * Thu May 14 20:58:21 UTC 2020
 */

package org.apache.commons.lang;

import org.junit.Test;
import static org.junit.Assert.*;
import static org.evosuite.runtime.EvoAssertions.*;
import java.util.Collection;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Locale;
import org.apache.commons.lang.LocaleUtils;
import org.evosuite.runtime.EvoRunner;
import org.evosuite.runtime.EvoRunnerParameters;
import org.junit.runner.RunWith;

@RunWith(EvoRunner.class) @EvoRunnerParameters(useVFS = true, useJEE = true) 
public class LocaleUtils_ESTest extends LocaleUtils_ESTest_scaffolding {

  @Test(timeout = 4000)
  public void test0()  throws Throwable  {
      LinkedList<Integer> linkedList0 = new LinkedList<Integer>();
      LinkedList<Object> linkedList1 = new LinkedList<Object>();
      Locale locale0 = LocaleUtils.toLocale("ne");
      LinkedList<Object> linkedList2 = new LinkedList<Object>();
      LinkedList<Locale.LanguageRange> linkedList3 = new LinkedList<Locale.LanguageRange>();
      LinkedList<Locale> linkedList4 = new LinkedList<Locale>();
      Locale.filter((List<Locale.LanguageRange>) linkedList3, (Collection<Locale>) linkedList4);
      LinkedList<Object> linkedList5 = new LinkedList<Object>();
      LinkedList<Object> linkedList6 = new LinkedList<Object>();
      Locale locale1 = Locale.forLanguageTag("");
      HashMap<String, List<String>> hashMap0 = new HashMap<String, List<String>>();
      locale0.getDisplayCountry(locale1);
      LinkedList<Locale> linkedList7 = new LinkedList<Locale>();
      // Undeclared exception!
      LocaleUtils.toLocale("yr_X/70G&|");
  }
}
