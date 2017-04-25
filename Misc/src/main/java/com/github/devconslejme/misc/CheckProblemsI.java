package com.github.devconslejme.misc;

import java.util.ArrayList;

public class CheckProblemsI {
	public static CheckProblemsI i(){return GlobalManagerI.i().get(CheckProblemsI.class);}
	
	public static interface ICheckProblems {
		/**
		 * Both params can be verified by checkers for matching clues to track and pin point problems
		 * that are not made 100% clear by some exceptions.
		 * 
		 * @param strExceptionMessage 
		 * @param thr 
		 * @return
		 */
		int checkProblems(String strExceptionMessage, Throwable thr);
	}
	
	private ArrayList<ICheckProblems> achkprbList = new ArrayList<ICheckProblems>();
//	private String	strExceptionMessage;
//	private Throwable	thr;
	
	public void addProblemsChecker(ICheckProblems chkprb){
		if(!achkprbList.contains(chkprb)){
			achkprbList.add(chkprb);
			MessagesI.i().debugInfo(this, "added", chkprb);
		}else{
			MessagesI.i().warnMsg(this, "already added", chkprb);
		}
	}
	
//	public void setProblemInfo(String strExceptionMessage, Throwable thr){
//		this.strExceptionMessage=strExceptionMessage;
//		this.thr=thr;
//	}
	
	public Integer checkProblems(String strExceptionMessage, Throwable thr){
		int i=0;
		for(ICheckProblems chkprb:achkprbList){
			i+=chkprb.checkProblems(strExceptionMessage, thr);
		}
		return i;
	}
	
//	Callable<Integer> call = new Callable<Integer>(){
//		@Override
//		public Integer call() throws Exception {
//			int i=0;
//			for(ICheckProblems chkprb:achkprbList){
//				if(chkprb.checkProblems(strExceptionMessage, thr)){
//					i++;
//				}
//			}
//			return i;
//		}
//		
//	};
//	
//	public Callable<Integer> getCheckProblemsCall(){
//		return call;
//	}
	
}
