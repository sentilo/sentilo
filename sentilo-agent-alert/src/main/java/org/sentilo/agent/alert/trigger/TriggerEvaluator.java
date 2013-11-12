/*
 * Sentilo
 *   
 * Copyright (C) 2013 Institut Municipal d’Informàtica, Ajuntament de  Barcelona.
 *   
 * This program is licensed and may be used, modified and redistributed under the
 * terms  of the European Public License (EUPL), either version 1.1 or (at your 
 * option) any later version as soon as they are approved by the European 
 * Commission.
 *   
 * Alternatively, you may redistribute and/or modify this program under the terms
 * of the GNU Lesser General Public License as published by the Free Software 
 * Foundation; either  version 3 of the License, or (at your option) any later 
 * version. 
 *   
 * Unless required by applicable law or agreed to in writing, software distributed
 * under the License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR 
 * CONDITIONS OF ANY KIND, either express or implied. 
 *   
 * See the licenses for the specific language governing permissions, limitations 
 * and more details.
 *   
 * You should have received a copy of the EUPL1.1 and the LGPLv3 licenses along 
 * with this program; if not, you may find them at: 
 *   
 *   https://joinup.ec.europa.eu/software/page/eupl/licence-eupl
 *   http://www.gnu.org/licenses/ 
 *   and 
 *   https://www.gnu.org/licenses/lgpl.txt
 */
package org.sentilo.agent.alert.trigger;

import java.math.BigDecimal;
import java.text.DecimalFormat;
import java.text.NumberFormat;
import java.text.ParseException;
import java.util.Date;
import java.util.Locale;

import org.sentilo.agent.alert.domain.Alarm;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.util.StringUtils;


public class TriggerEvaluator {	
	private final Logger logger = LoggerFactory.getLogger(TriggerEvaluator.class);
	private static DecimalFormat decimalFormat;
	
	private String lastAcceptedValue;
	private Date tsLastAcceptedValue = new Date();
	
	private static final String TEMPLATE_MESSAGE = "Alarm %s: value %s from the sensor %s verifies the restriction: %s";
	private static final String TEMPLATE_NO_NUMBER_MESSAGE = "Alarm %s: value %s from the sensor %s must be a number";
	private static final String TEMPLATE_GT_MESSAGE = "Greater than %s";
	private static final String TEMPLATE_GTE_MESSAGE = "Greather than or equals to %s";
	private static final String TEMPLATE_LT_MESSAGE = "Less than %s";
	private static final String TEMPLATE_LTE_MESSAGE = "Less than or equals to %s";
	private static final String TEMPLATE_EQ_MESSAGE = "Equals to %s";
	private static final String TEMPLATE_CHANGE_MESSAGE = "Value has changed";
	private static final String TEMPLATE_CHANGE_DELTA_MESSAGE = "Value variation is greater than %s percent";
	private static final String TEMPLATE_FROZEN_ALARM = "Alarm %s: sensor %s is frozen. It has not been updated for the last %s minute(s). Idle time: %s minute(s)";
	
	public TriggerResult evaluate(Alarm alarm, String value){
		//GT, GTE, LT, LTE, EQ, CHANGE, CHANGE_DELTA
		logger.debug("Evaluating alarm {} for sensor value {}", alarm.getId(), value);
		
		TriggerResult result = null;
		try{
			switch(alarm.getTrigger()){
				case GT:
					result = evaluateGreaterThanTrigger(alarm, value);
					break;
				case GTE:
					result = evaluateGreaterThanOrEqualsTrigger(alarm, value);
					break;
				case LT:
					result = evaluateLessThanTrigger(alarm, value);
					break;
				case LTE:
					result = evaluateLessThanOrEqualsTrigger(alarm, value);
					break;
				case EQ:
					result = evaluateEqualsTrigger(alarm, value);
					break;
				case CHANGE:
					result = evaluateChangeTrigger(alarm, value);
					break;
				case CHANGE_DELTA:
					result = evaluateChangeDeltaTrigger(alarm, value);
					break;
				default:
					result = new TriggerResult();
					break;			
			}
		}catch(ParseException pe){
			// Esta excepcion no deberia ocurrir nunca ya que al dar de alta la alarma en el catalogo se valida 
			// el formato numerico de la expresion, y antes de aplicar cualquier evaluacion se valida que el valor
			// del sensor sea numerico. Pero es mejor controlarla que retornar una excepcion o ignorarla.
			result = new TriggerResult(String.format(TEMPLATE_NO_NUMBER_MESSAGE, alarm.getId(), value, alarm.getSensorId()));
		}
		
		logger.debug("Evaluation result was {}", result.triggerConditionChecked());
		
		return result;
	}
	
	public TriggerResult checkFrozen(Alarm alarm){
		TriggerResult result = null;
		logger.debug("Evaluating frozen alarm {}", alarm.getId());		
		
		try{
			result =  evaluateFrozenTrigger(alarm);			
		}catch(ParseException pe){
			// Idem al comentario anterior: esta excepcion no debería ocurrir pero es mas limpio no propagarla.
			result = new TriggerResult(String.format(TEMPLATE_NO_NUMBER_MESSAGE, alarm.getId(), alarm.getExpression(), alarm.getSensorId()));
		}
							
		logger.debug("Evaluation result was {}", result.triggerConditionChecked());
		
		return result;
	}
	
	public void setLastAcceptedValue(String lastAcceptedValue) {
		this.lastAcceptedValue = lastAcceptedValue;
		this.tsLastAcceptedValue = new Date();
	}
	
	private boolean isNumberValue(String value){
		return value.matches("[-+]?\\d+(\\.\\d+)?");
	}
	
	
	private int compareNumbers(String sensorValue, String valueToCompare) throws ParseException{		
		BigDecimal bdValue = transformNumber(sensorValue);
		BigDecimal limit = transformNumber(valueToCompare);
		return bdValue.compareTo(limit);						
	}
	
	private String buildErrorMessage(String templateErrorMessage, Alarm alarm, String value){
		String expressionMessage = String.format(templateErrorMessage, alarm.getExpression());						
		return String.format(TEMPLATE_MESSAGE, alarm.getId(), value, alarm.getSensorId(), expressionMessage);				
	}
	
	
	private TriggerResult evaluateGreaterThanTrigger(Alarm alarm, String value) throws ParseException{
		TriggerResult result = null;
		
		if(!isNumberValue(value)){			
			result = new TriggerResult(String.format(TEMPLATE_NO_NUMBER_MESSAGE, alarm.getId(), value, alarm.getSensorId()));
		}else{																		
			String errorMessage = buildErrorMessage(TEMPLATE_GT_MESSAGE, alarm, value);
			result = (compareNumbers(value, alarm.getExpression()) == 1)?new TriggerResult(errorMessage):new TriggerResult();		
		}
		
		return result;
	}
	
	private TriggerResult evaluateGreaterThanOrEqualsTrigger(Alarm alarm, String value) throws ParseException{
		TriggerResult result = null;
		
		if(!isNumberValue(value)){			
			result = new TriggerResult(String.format(TEMPLATE_NO_NUMBER_MESSAGE, alarm.getId(), value, alarm.getSensorId()));
		}else{															
			String errorMessage = buildErrorMessage(TEMPLATE_GTE_MESSAGE, alarm, value);
			result = (compareNumbers(value, alarm.getExpression()) != 1)?new TriggerResult(errorMessage):new TriggerResult();					
		}
		
		return result;
	}
	
	private TriggerResult evaluateLessThanTrigger(Alarm alarm, String value) throws ParseException{
		TriggerResult result = null;
		
		if(!isNumberValue(value)){			
			result = new TriggerResult(String.format(TEMPLATE_NO_NUMBER_MESSAGE, alarm.getId(), value, alarm.getSensorId()));
		}else{
			String errorMessage = buildErrorMessage(TEMPLATE_LT_MESSAGE, alarm, value);
			result = (compareNumbers(value, alarm.getExpression()) == -1)?new TriggerResult(errorMessage):new TriggerResult();						
		}
		
		return result;
	}
	
	private TriggerResult evaluateLessThanOrEqualsTrigger(Alarm alarm, String value) throws ParseException{
		TriggerResult result = null;
		
		if(!isNumberValue(value)){			
			result = new TriggerResult(String.format(TEMPLATE_NO_NUMBER_MESSAGE, alarm.getId(), value, alarm.getSensorId()));
		}else{			
			String errorMessage = buildErrorMessage(TEMPLATE_LTE_MESSAGE, alarm, value);
			result = (compareNumbers(value, alarm.getExpression())!=1)?new TriggerResult(errorMessage):new TriggerResult();						
		}
		
		return result;
	}
	
	private TriggerResult evaluateEqualsTrigger(Alarm alarm, String value){
		TriggerResult result = null;
		String errorMessage = buildErrorMessage(TEMPLATE_EQ_MESSAGE, alarm, value);				
		result = (value.equals(alarm.getExpression())?new TriggerResult(errorMessage):new TriggerResult());			
		
		return result;
	}
	
	private TriggerResult evaluateChangeTrigger(Alarm alarm, String value){
		TriggerResult result = null;
		String errorMessage = buildErrorMessage(TEMPLATE_CHANGE_MESSAGE, alarm, value);				
		if(StringUtils.hasText(lastAcceptedValue)){
			result = (value.equals(lastAcceptedValue)?new TriggerResult():new TriggerResult(errorMessage));
		}else{
			result = new TriggerResult();
		}
		
		return result;
	}
	
	private TriggerResult evaluateChangeDeltaTrigger(Alarm alarm, String value) throws ParseException{
		TriggerResult result = null;
		
		// La comparacion consiste en ver si la variacion entre el valor recibido (B) y el ultimo valor almacenado (A)
		// es superior al % indicado
		// Variacion = (|A-B|/|A|)*100
		if(StringUtils.hasText(lastAcceptedValue)){
			
			if(!isNumberValue(value)){			
				result = new TriggerResult(String.format(TEMPLATE_NO_NUMBER_MESSAGE, alarm.getId(), value, alarm.getSensorId()));
			}else{
				float limit = transformNumber(alarm.getExpression()).floatValue();
				float absValue = Math.abs(transformNumber(value).floatValue());
				float absLastAccepted = Math.abs(transformNumber(lastAcceptedValue).floatValue());
				
				float variation = ((absLastAccepted - absValue) / absLastAccepted) * 100;
				
				String errorMessage = buildErrorMessage(TEMPLATE_CHANGE_DELTA_MESSAGE, alarm, value);				
				result = (variation > limit?new TriggerResult(errorMessage): new TriggerResult());
			}						
		}else{
			result = new TriggerResult();
		}
		
		return result;
	}
	
	private TriggerResult evaluateFrozenTrigger(Alarm alarm) throws ParseException{
		TriggerResult result = null;		
						
		// Hemos de calcular los minutos de diferencia entre la fecha actual y la fecha tsLastAcceptedValue y ver si es superior a los minutos 
		// asociados a la alarma.
		// Habrá que tocar tb el listado de alarmas del catálogo para añadir la nueva alarma.
		float maxFrozenMinutes = transformNumber(alarm.getExpression()).longValue();
		Date date = new Date();
		float frozenMinutes = (date.getTime() - tsLastAcceptedValue.getTime())/(1000*60);
		
		if(frozenMinutes > maxFrozenMinutes){
			String errorMessage = String.format(TEMPLATE_FROZEN_ALARM, alarm.getId(), alarm.getSensorId(), alarm.getExpression(), frozenMinutes);
			result = new TriggerResult(errorMessage);
		}else{
			result = new TriggerResult();
		}				
		
		return result;
	}
	
	private BigDecimal transformNumber(String value) throws ParseException{
		return (BigDecimal)getDecimalFormat().parse(value);		
	}
	
	private DecimalFormat getDecimalFormat(){
		if(decimalFormat == null){
			decimalFormat = (DecimalFormat) NumberFormat.getInstance(Locale.ENGLISH);
			decimalFormat.setParseBigDecimal(true);
		}		
		return decimalFormat;
	}

	
	
}
