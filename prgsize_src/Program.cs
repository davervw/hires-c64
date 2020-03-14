using System;
using System.IO;

namespace prgsize
{
    class Program
    {
        static void Main(string[] args)
        {
            try
            {
                FileInfo fi = new FileInfo(args[0]);
                UInt16 size = Convert.ToUInt16(fi.Length - 2);  // subtract 2 for starting embedded address
                byte sizelo = Convert.ToByte(size % 256);
                byte sizehi = Convert.ToByte(size / 256);
                using (Stream stream = Console.OpenStandardOutput())
                {
                    stream.WriteByte(sizelo);
                    stream.WriteByte(sizehi);
                }
            }
            catch (Exception ex)
            {
                Console.Error.WriteLine(ex.ToString());
            }
        }
     }
}
