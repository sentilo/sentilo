/*
 * Sentilo
 * 
 * Copyright (C) 2013 Institut Municipal d’Informàtica, Ajuntament de Barcelona.
 * 
 * This program is licensed and may be used, modified and redistributed under the terms of the
 * European Public License (EUPL), either version 1.1 or (at your option) any later version as soon
 * as they are approved by the European Commission.
 * 
 * Alternatively, you may redistribute and/or modify this program under the terms of the GNU Lesser
 * General Public License as published by the Free Software Foundation; either version 3 of the
 * License, or (at your option) any later version.
 * 
 * Unless required by applicable law or agreed to in writing, software distributed under the License
 * is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express
 * or implied.
 * 
 * See the licenses for the specific language governing permissions, limitations and more details.
 * 
 * You should have received a copy of the EUPL1.1 and the LGPLv3 licenses along with this program;
 * if not, you may find them at:
 * 
 * https://joinup.ec.europa.eu/software/page/eupl/licence-eupl http://www.gnu.org/licenses/ and
 * https://www.gnu.org/licenses/lgpl.txt
 */
package org.sentilo.web.catalog.utils;

import org.springframework.ui.Model;

public abstract class ModelUtils {

  public static void addConfirmationMessageTo(final Model model, final String messageKey) {
    model.addAttribute(Constants.MODEL_CONFIRMATION_MESSAGE_KEY, messageKey);
  }

  public static void setCreateMode(final Model model) {
    model.addAttribute(Constants.MODEL_MODE, Constants.MODE_CREATE);
  }

  public static void setEditMode(final Model model) {
    model.addAttribute(Constants.MODEL_MODE, Constants.MODE_EDIT);
  }

  public static void setDetailMode(final Model model) {
    model.addAttribute(Constants.MODEL_MODE, Constants.MODE_DETAIL);
  }

  public static void setDataMode(final Model model) {
    model.addAttribute(Constants.MODEL_MODE, Constants.MODE_DATA);
  }

  public static void addErrorMessageTo(final Model model, final String message) {
    model.addAttribute(Constants.MODEL_ERROR_MESSAGE, message);
  }

  public static void addOpenedTabTo(final Model model, final int index) {
    model.addAttribute(Constants.MODEL_OPENED_TAB, index);
  }

  public static void addActiveMenuTo(final Model model, final String menu) {
    model.addAttribute(Constants.MODEL_ACTIVE_MENU, menu);
  }

  private ModelUtils() {
    // this prevents even the native class from calling this ctor as well :
    throw new AssertionError();
  }
}
