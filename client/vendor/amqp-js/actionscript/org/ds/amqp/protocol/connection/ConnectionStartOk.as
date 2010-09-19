/**
---------------------------------------------------------------------------

Copyright (c) 2009 Dan Simpson

Auto-Generated @ Wed Aug 26 19:21:28 -0700 2009.  Do not edit this file, extend it you must.

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in
all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
THE SOFTWARE.

---------------------------------------------------------------------------
**/

/*
Documentation

    This method selects a SASL security mechanism. ASL uses SASL
    (RFC2222) to negotiate authentication and encryption.
  
*/
package org.ds.amqp.protocol.connection
{
	import flash.utils.ByteArray;
	import org.ds.amqp.datastructures.*;
	import org.ds.amqp.protocol.Method;
	import org.ds.amqp.transport.Buffer;
	
	public dynamic class ConnectionStartOk extends Method
	{
		public static var EVENT:String = "10/11";

		//
		public var clientProperties        :FieldTable = new FieldTable();

		//
		public var mechanism               :String = "";

		//
		public var response                :FieldTable = new FieldTable();

		//
		public var locale                  :String = "";

		
		public function ConnectionStartOk() {
			_classId  = 10;
			_methodId = 11;
			
			_synchronous = true;

		}


		public override function writeArguments(buf:Buffer):void {

			buf.writeTable(this.clientProperties);
			buf.writeShortString(this.mechanism);
			buf.writeTable(this.response);
			buf.writeShortString(this.locale);
		}
		
		public override function readArguments(buf:Buffer):void {

			this.clientProperties = buf.readTable();
			this.mechanism        = buf.readShortString();
			this.response         = buf.readTable();
			this.locale           = buf.readShortString();
		}
		
		public override function print():void {
			var props:Array = [
				"clientProperties","mechanism","response","locale"
			];
			printObj("ConnectionStartOk", props);
		}

	}
}