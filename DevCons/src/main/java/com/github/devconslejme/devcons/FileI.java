/* 
	Copyright (c) 2017, Henrique Abdalla <https://github.com/AquariusPower><https://sourceforge.net/u/teike/profile/>
	
	All rights reserved.

	Redistribution and use in source and binary forms, with or without modification, are permitted 
	provided that the following conditions are met:

	1.	Redistributions of source code must retain the above copyright notice, this list of conditions 
		and the following disclaimer.

	2.	Redistributions in binary form must reproduce the above copyright notice, this list of conditions 
		and the following disclaimer in the documentation and/or other materials provided with the distribution.
	
	3.	Neither the name of the copyright holder nor the names of its contributors may be used to endorse 
		or promote products derived from this software without specific prior written permission.

	THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED 
	WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A 
	PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR 
	ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT 
	LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS 
	INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, 
	OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN 
	IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*/

package com.github.devconslejme.devcons;

import java.io.File;
import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.util.List;

import com.github.devconslejme.misc.DetailedException;
import com.github.devconslejme.misc.GlobalManagerI;
import com.github.devconslejme.misc.MessagesI;
import com.google.common.io.Files;
import com.jme3.app.Application;
import com.jme3.system.JmeSystem;
import com.jme3.system.JmeSystem.StorageFolderType;


/**
 * @author Henrique Abdalla <https://github.com/AquariusPower><https://sourceforge.net/u/teike/profile/>
 */
public class FileI {
//	private File	flBasePath;
	private File	flStorageFolder;
	private Application	app;

	public static FileI i(){return GlobalManagerI.i().get(FileI.class);}
	
//	public void configure(File flBasePath){
	public void configure(){
		app = GlobalManagerI.i().get(Application.class);
//		setBasePath(flBasePath);
		flStorageFolder = new File(
				JmeSystem.getStorageFolder(StorageFolderType.Internal),
				app.getClass().getPackage().getName().replace(".",File.separator) //package of Application class
					+File.separator+app.getClass().getSimpleName() //Application class
					+File.separator+DevConsPluginStateI.class.getSimpleName() //DevCons plugin
			);
	}
	
	public void appendLine(File fl, String str){
		try {
			if(!fl.exists())fl.getParentFile().mkdirs();
			Files.append(str+"\n", fl, StandardCharsets.UTF_8);
		} catch (IOException e) {
			LoggingI.i().logExceptionEntry(e, null, true); //also prevents recursiveness
		}
	}

	public List<String> readAllLines(File fl) {
		try {
			return Files.readLines(fl, StandardCharsets.UTF_8);
		} catch (IOException e) {
			LoggingI.i().logExceptionEntry(e, null, true); //also prevents recursiveness
		}
		
		return null;
	}

//	public void setBasePath(File flBasePath) {
//		DetailedException.assertNotAlreadySet(this.flBasePath, flBasePath, this);
//		this.flBasePath = flBasePath;
//	}
	
	/**
	 * in the base application user data path
	 * @param strFile
	 * @return
	 */
	public File createNewFileHandler(String strFile,boolean bFileCanExist){
//		DetailedException.assertIsTrue("base path", this.flBasePath, flBasePath, this);
//		if(flBasePath==null)MessagesI.i().warnMsg(this, "base path not set", strFile);
		
		File fl= new File(flStorageFolder, strFile);
		if(fl.exists() && !bFileCanExist){
			throw new DetailedException("file already exists",fl);
		}
		
		return fl;
	}
	
	public File getStorageFolder() {
		return flStorageFolder;
	}

	public File createNewFile(Object objForConcreteClassSimpleName,String strExt,boolean bFileCanExist){
		return createNewFileHandler(objForConcreteClassSimpleName.getClass().getSimpleName()+"."+strExt, bFileCanExist);
	}
}
